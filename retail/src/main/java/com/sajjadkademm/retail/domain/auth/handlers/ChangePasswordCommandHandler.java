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
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;

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
    private final GlobalAuditService globalAuditService;

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
            globalAuditService.auditSecurityEvent(
                    null, // No organization context for password change
                    AuditAction.PASSWORD_CHANGE,
                    String.format("User %s (%s) changed their password from IP %s", 
                            currentUser.getName(), currentUser.getEmail(), command.getClientIp()),
                    currentUser.getId(),
                    currentUser.getName(),
                    String.format("User-Agent: %s", command.getUserAgent())
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
            globalAuditService.auditSecurityEvent(
                    null,
                    AuditAction.SUSPICIOUS_ACTIVITY,
                    String.format("Failed password change attempt for user ID %s from IP %s: %s", 
                            command.getUserId(), command.getClientIp(), e.getMessage()),
                    command.getUserId(),
                    "Unknown", // We don't have the user name in case of failure
                    String.format("User-Agent: %s", command.getUserAgent())
            );
            
            throw e;
        }
    }

    @Override
    public Class<ChangePasswordCommand> getCommandType() {
        return ChangePasswordCommand.class;
    }
}