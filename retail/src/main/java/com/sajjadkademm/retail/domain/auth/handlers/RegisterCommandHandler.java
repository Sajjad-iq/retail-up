package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.commands.RegisterCommand;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.application.services.users.UserService;
import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

/**
 * Command handler for user registration operations
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class RegisterCommandHandler implements CommandHandler<RegisterCommand, LoginResponse> {

    private final UserService userService;
    private final BCryptPasswordEncoder passwordEncoder;
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final GlobalAuditService globalAuditService;

    @Override
    public LoginResponse handle(RegisterCommand command) throws Exception {
        log.debug("Processing registration command for user: {}", command.getRequest().getName());

        try {
            // Create new user
            User newUser = User.builder()
                    .name(command.getRequest().getName())
                    .email(command.getRequest().getEmail())
                    .phone(command.getRequest().getPhone())
                    .password(passwordEncoder.encode(command.getRequest().getPassword()))
                    .build();

            // Save user using UserService
            User savedUser = userService.createUser(newUser);

            // Generate JWT token
            String token = jwtUtil.generateToken(
                    savedUser.getId(), 
                    savedUser.getPhone(), 
                    savedUser.getName(),
                    savedUser.getEmail()
            );

            // Audit successful registration
            globalAuditService.auditSecurityEvent(
                    null, // No organization context for registration
                    AuditAction.USER_CREATE,
                    String.format("New user registered: %s (%s) from IP %s", 
                            savedUser.getName(), savedUser.getEmail(), command.getClientIp()),
                    savedUser.getId(),
                    savedUser.getName(),
                    String.format("User-Agent: %s", command.getUserAgent())
            );

            log.info("User registration successful: {} ({})", savedUser.getName(), savedUser.getEmail());

            return LoginResponse.builder()
                    .token(token)
                    .userId(savedUser.getId())
                    .name(savedUser.getName())
                    .phone(savedUser.getPhone())
                    .email(savedUser.getEmail())
                    .status(savedUser.getStatus())
                    .accountType(savedUser.getAccountType())
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_REGISTRATION_SUCCESSFUL
                                    .getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Registration failed for user: {} - {}", command.getRequest().getName(), e.getMessage());
            
            // Audit failed registration attempt
            globalAuditService.auditSecurityEvent(
                    null,
                    AuditAction.SUSPICIOUS_ACTIVITY,
                    String.format("Failed registration attempt for %s from IP %s: %s", 
                            command.getRequest().getName(), command.getClientIp(), e.getMessage()),
                    null, // No user ID for failed registration
                    command.getRequest().getName(),
                    String.format("User-Agent: %s", command.getUserAgent())
            );
            
            throw e;
        }
    }

    @Override
    public Class<RegisterCommand> getCommandType() {
        return RegisterCommand.class;
    }
}