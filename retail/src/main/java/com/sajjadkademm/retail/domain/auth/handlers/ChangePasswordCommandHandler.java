package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.commands.ChangePasswordCommand;
import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.domain.auth.validation.AuthValidator;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

/**
 * Command handler for password change operations
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ChangePasswordCommandHandler implements CommandHandler<ChangePasswordCommand, AuthResponse> {

    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder;
    private final LocalizedErrorService localizedErrorService;
    private final AuthValidator authValidator;
    private final GlobalAuditRepository auditRepository;

    @Override
    public AuthResponse handle(ChangePasswordCommand command) throws Exception {
        log.debug("Processing change password command for user: {}", command.getUserId());

        try {
            // Get current user directly from repository to avoid circular dependency
            User currentUser = userRepository.findById(command.getUserId())
                .orElseThrow(() -> new NotFoundException("User not found: " + command.getUserId()));

            // Validate old password using AuthValidator
            authValidator.validateOldPassword(command.getOldPassword(), currentUser);

            // Update password directly
            currentUser.setPassword(passwordEncoder.encode(command.getNewPassword()));
            userRepository.save(currentUser);

            // Audit password change
            auditSecurityEvent(
                    null, // No organization context for password change
                    AuditAction.PASSWORD_CHANGE,
                    String.format("User %s (%s) changed their password from IP %s", 
                            currentUser.getName(), currentUser.getEmail(), command.getClientIp()),
                    currentUser.getId(),
                    currentUser.getName(),
                    String.format("User-Agent: %s", command.getUserAgent()),
                    command.getClientIp(),
                    command.getUserAgent()
            );

            log.info("Password changed successfully for user: {} ({})", currentUser.getName(), currentUser.getEmail());

            return AuthResponse.builder()
                    .success(true)
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_PASSWORD_CHANGED_SUCCESSFULLY
                                    .getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Password change failed for user: {} - {}", command.getUserId(), e.getMessage());
            
            // Audit failed password change attempt
            auditSecurityEvent(
                    null,
                    AuditAction.SUSPICIOUS_ACTIVITY,
                    String.format("Failed password change attempt for user ID %s from IP %s: %s", 
                            command.getUserId(), command.getClientIp(), e.getMessage()),
                    command.getUserId(),
                    "Unknown", // We don't have the user name in case of failure
                    String.format("User-Agent: %s", command.getUserAgent()),
                    command.getClientIp(),
                    command.getUserAgent()
            );
            
            throw e;
        }
    }

    @Override
    public Class<ChangePasswordCommand> getCommandType() {
        return ChangePasswordCommand.class;
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