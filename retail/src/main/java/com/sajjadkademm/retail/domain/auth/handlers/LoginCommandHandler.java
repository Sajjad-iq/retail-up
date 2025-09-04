package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.commands.LoginCommand;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.application.services.users.UserService;
import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.domain.auth.validation.AuthValidator;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;

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

    private final UserService userService;
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final AuthValidator authValidator;
    private final GlobalAuditService globalAuditService;

    @Override
    public LoginResponse handle(LoginCommand command) throws Exception {
        log.debug("Processing login command for user: {}", command.getRequest().getEmailOrPhone());

        try {
            // Validate login credentials using AuthValidator
            User user = authValidator.validateLoginCredentials(
                    command.getRequest().getEmailOrPhone(), 
                    command.getRequest().getPassword()
            );

            // Update last login time
            user.setLastLoginAt(LocalDateTime.now());
            userService.updateUser(user.getId(), user);

            // Generate JWT token
            String token = jwtUtil.generateToken(
                    user.getId(), 
                    user.getPhone(), 
                    user.getName(), 
                    user.getEmail()
            );

            // Audit successful login
            globalAuditService.auditSecurityEvent(
                    null, // No organization context for login
                    AuditAction.USER_LOGIN,
                    String.format("User %s (%s) logged in successfully from IP %s", 
                            user.getName(), user.getEmail(), command.getClientIp()),
                    user.getId(),
                    user.getName(),
                    String.format("User-Agent: %s", command.getUserAgent())
            );

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
            globalAuditService.auditSecurityEvent(
                    null,
                    AuditAction.FAILED_LOGIN,
                    String.format("Failed login attempt for %s from IP %s: %s", 
                            command.getRequest().getEmailOrPhone(), command.getClientIp(), e.getMessage()),
                    null, // No user ID for failed login
                    command.getRequest().getEmailOrPhone(),
                    String.format("User-Agent: %s", command.getUserAgent())
            );
            
            throw e;
        }
    }

    @Override
    public Class<LoginCommand> getCommandType() {
        return LoginCommand.class;
    }
}