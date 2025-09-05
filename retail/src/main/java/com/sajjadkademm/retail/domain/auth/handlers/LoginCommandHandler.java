package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.commands.LoginCommand;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.domain.auth.validation.AuthValidator;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

/**
 * Command handler for user login operations
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LoginCommandHandler implements CommandHandler<LoginCommand, LoginResponse> {

    private final UserRepository userRepository;
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final AuthValidator authValidator;
    private final GlobalAuditRepository auditRepository;

    @Override
    public LoginResponse handle(LoginCommand command) throws Exception {
        log.debug("Processing login command for user: {}", command.getRequest().getEmailOrPhone());

        try {
            // Validate login credentials using AuthValidator
            User user = authValidator.validateLoginCredentials(
                    command.getRequest().getEmailOrPhone(), 
                    command.getRequest().getPassword()
            );

            // Update last login time directly to avoid circular dependency
            user.setLastLoginAt(LocalDateTime.now());
            userRepository.save(user);

            // Generate JWT token
            String token = jwtUtil.generateToken(
                    user.getId(), 
                    user.getPhone(), 
                    user.getName(), 
                    user.getEmail()
            );

            // Audit successful login
            auditSecurityEvent(null, AuditAction.USER_LOGIN, 
                    String.format("User %s (%s) logged in successfully from IP %s", 
                            user.getName(), user.getEmail(), command.getClientIp()),
                    user.getId(), user.getName(), 
                    String.format("User-Agent: %s", command.getUserAgent()),
                    command.getClientIp(), command.getUserAgent());

            log.info("User login successful: {} ({})", user.getName(), user.getEmail());

            return LoginResponse.builder()
                    .token(token)
                    .userId(user.getId())
                    .name(user.getName())
                    .phone(user.getPhone())
                    .email(user.getEmail())
                    .status(user.getStatus())
                    .accountType(user.getAccountType())
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_LOGIN_SUCCESSFUL.getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Login failed for user: {} - {}", command.getRequest().getEmailOrPhone(), e.getMessage());
            
            // Audit failed login attempt
            auditSecurityEvent(null, AuditAction.FAILED_LOGIN,
                    String.format("Failed login attempt for %s from IP %s: %s", 
                            command.getRequest().getEmailOrPhone(), command.getClientIp(), e.getMessage()),
                    null, command.getRequest().getEmailOrPhone(),
                    String.format("User-Agent: %s", command.getUserAgent()),
                    command.getClientIp(), command.getUserAgent());
            
            throw e;
        }
    }

    @Override
    public Class<LoginCommand> getCommandType() {
        return LoginCommand.class;
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