package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.commands.RegisterCommand;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;
import com.sajjadkademm.retail.domain.shared.validation.PhoneValidator;
import com.sajjadkademm.retail.domain.shared.validation.EmailValidator;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.shared.enums.AccountType;
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

    private final UserRepository userRepository;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;
    private final BCryptPasswordEncoder passwordEncoder;
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final GlobalAuditService globalAuditService;

    @Override
    public LoginResponse handle(RegisterCommand command) throws Exception {
        log.debug("Processing registration command for user: {}", command.getRequest().getName());

        try {
            // Validate user data (same as CreateUserCommandHandler)
            if (command.getRequest().getName() == null || command.getRequest().getName().trim().isEmpty()) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
            }

            // Validate phone format and uniqueness
            phoneValidator.validatePhoneFormatAndUniqueness(command.getRequest().getPhone(),
                    (phone) -> userRepository.existsByPhone(command.getRequest().getPhone()));

            // Validate email format and uniqueness if provided
            if (command.getRequest().getEmail() != null) {
                emailValidator.validateEmailFormatAndUniqueness(command.getRequest().getEmail(),
                        (email) -> userRepository.existsByEmail(command.getRequest().getEmail()));
            }

            // Create new user
            User newUser = User.builder()
                    .name(command.getRequest().getName())
                    .email(command.getRequest().getEmail())
                    .phone(command.getRequest().getPhone())
                    .password(passwordEncoder.encode(command.getRequest().getPassword()))
                    .status(UserStatus.ACTIVE)
                    .accountType(AccountType.USER)
                    .build();

            // Save user directly to avoid circular dependency
            User savedUser = userRepository.save(newUser);

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