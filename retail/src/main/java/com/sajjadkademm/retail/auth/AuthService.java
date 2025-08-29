package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.config.locales.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.config.utils.JwtUtil;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.auth.dto.AuthResponse;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.auth.validator.AuthValidator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder;
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final AuthValidator authValidator;

    /**
     * Authenticate user with email/phone and password
     */
    public LoginResponse login(LoginRequest request) {
        // Validate login credentials using AuthValidator
        User user = authValidator.validateLoginCredentials(request.getEmailOrPhone(), request.getPassword());

        userValidator.assertUserIsActive(user);

        // Update last login time
        user.setLastLoginAt(LocalDateTime.now());
        userRepository.save(user);

        // Generate JWT token
        String token = jwtUtil.generateToken(user.getId(), user.getPhone(), user.getName());

        return LoginResponse.builder()
                .token(token)
                .userId(user.getId())
                .name(user.getName())
                .phone(user.getPhone())
                .status(user.getStatus())
                .accountType(user.getAccountType())
                .message(localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_LOGIN_SUCCESSFUL.getMessage()))
                .build();
    }

    /**
     * Register new user
     */
    public LoginResponse register(RegisterRequest request) {
        // Create new user
        User newUser = User.builder()
                .name(request.getName())
                .email(request.getEmail())
                .phone(request.getPhone())
                .password(passwordEncoder.encode(request.getPassword()))
                .status(UserStatus.ACTIVE)
                .accountType(AccountType.USER)
                .build();

        // Save user using UserService
        User savedUser = userService.createUser(newUser);

        // Generate JWT token
        String token = jwtUtil.generateToken(savedUser.getId(), savedUser.getPhone(), savedUser.getName());

        return LoginResponse.builder()
                .token(token)
                .userId(savedUser.getId())
                .name(savedUser.getName())
                .phone(savedUser.getPhone())
                .status(savedUser.getStatus())
                .accountType(savedUser.getAccountType())
                .message(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_REGISTRATION_SUCCESSFUL.getMessage()))
                .build();
    }

    /**
     * Change current user password and return response
     */
    public AuthResponse changePasswordWithResponse(String oldPassword, String newPassword) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Validate old password using AuthValidator
        authValidator.validateOldPassword(oldPassword, currentUser);

        // Update password
        currentUser.setPassword(passwordEncoder.encode(newPassword));
        userService.updateUser(currentUser.getId(), currentUser);

        return AuthResponse.builder()
                .success(true)
                .message(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_PASSWORD_CHANGED_SUCCESSFULLY.getMessage()))
                .build();
    }

    /**
     * Refresh/validate JWT token and return user information if valid
     */
    public LoginResponse refreshToken(String authHeader) {

        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_HEADER_INVALID.getMessage()));
        }

        String token = authHeader.substring(7);

        try {
            if (!jwtUtil.validateToken(token)) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_INVALID_TOKEN.getMessage()));
            }

            // Extract user information from token
            String userId = jwtUtil.extractUserId(token);

            // Verify user still exists and is active using AuthValidator
            User user = authValidator.validateTokenUser(userId);

            String newToken = jwtUtil.generateToken(user.getId(), user.getPhone(), user.getName());

            return LoginResponse.builder()
                    .token(newToken)
                    .userId(user.getId())
                    .name(user.getName())
                    .phone(user.getPhone())
                    .status(user.getStatus())
                    .accountType(user.getAccountType())
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_TOKEN_VALID.getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Error validating token and getting user info: {}", e.getMessage());
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_INVALID_TOKEN.getMessage()));
        }
    }

    /**
     * Check if user exists by email or phone
     */
    public boolean userExists(String emailOrPhone) {
        return authValidator.userExists(emailOrPhone);
    }

}