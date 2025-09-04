package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.queries.ValidateTokenQuery;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Query handler for JWT token validation and refresh
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ValidateTokenQueryHandler implements QueryHandler<ValidateTokenQuery, LoginResponse> {

    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final GlobalAuditRepository auditRepository;

    @Override
    public LoginResponse handle(ValidateTokenQuery query) throws Exception {
        log.debug("Processing validate token query for user: {}", query.getUserId());

        try {
            if (query.getUserId() == null) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_INVALID_TOKEN.getMessage()));
            }

            // Validate user is active directly to avoid circular dependency
            User user = userValidator.validateUserActive(query.getUserId());

            // Generate new token
            String newToken = jwtUtil.generateToken(
                    user.getId(), 
                    user.getPhone(), 
                    user.getName(),
                    user.getEmail()
            );

            // Audit token validation
            auditSecurityEvent(
                    null, // No organization context for token validation
                    AuditAction.VIEW,
                    String.format("JWT token validated and refreshed for user %s (%s) from IP %s", 
                            user.getName(), user.getEmail(), query.getClientIp()),
                    user.getId(),
                    user.getName(),
                    String.format("User-Agent: %s", query.getUserAgent()),
                    query.getClientIp(),
                    query.getUserAgent()
            );

            log.debug("Token validated and refreshed for user: {} ({})", user.getName(), user.getEmail());

            return LoginResponse.builder()
                    .token(newToken)
                    .userId(user.getId())
                    .name(user.getName())
                    .phone(user.getPhone())
                    .email(user.getEmail())
                    .status(user.getStatus())
                    .accountType(user.getAccountType())
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_TOKEN_VALID.getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Error validating token for user {}: {}", query.getUserId(), e.getMessage());
            
            // Audit failed token validation
            auditSecurityEvent(
                    null,
                    AuditAction.ACCESS_DENIED,
                    String.format("Failed token validation for user ID %s from IP %s: %s", 
                            query.getUserId(), query.getClientIp(), e.getMessage()),
                    query.getUserId(),
                    "Unknown", // We don't have the user name in case of failure
                    String.format("User-Agent: %s", query.getUserAgent()),
                    query.getClientIp(),
                    query.getUserAgent()
            );
            
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_INVALID_TOKEN.getMessage()));
        }
    }

    @Override
    public Class<ValidateTokenQuery> getQueryType() {
        return ValidateTokenQuery.class;
    }

    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    private void auditSecurityEvent(String organizationId, AuditAction action, String description,
            String userId, String userName, String additionalInfo, String clientIp, String userAgent) {
        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(EntityType.USER)
                    .entityId(userId)
                    .entityName(userName)
                    .action(action)
                    .description(description)
                    .businessProcess("Security Management")
                    .oldValue(additionalInfo) // Store additional security context
                    .performedBy(null) // May be null for failed logins
                    .sourceIp(clientIp)
                    .userAgent(userAgent)
                    .isSensitive(true) // All security events are sensitive
                    .build();

            auditRepository.save(auditLog);

            log.info("Security audit logged: {} for user {} from IP {}", action, userName, clientIp);

        } catch (Exception e) {
            log.error("Failed to log security audit for action {}: {}", action, e.getMessage(), e);
        }
    }
}