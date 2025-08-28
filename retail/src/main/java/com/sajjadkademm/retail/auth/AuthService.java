package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.config.locales.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.config.utils.JwtUtil;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.auth.dto.AuthResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
    private final JwtUtil jwtUtil;
    private final LocalizedErrorService localizedErrorService;

    /**
     * Authenticate user with email/phone and password
     */
    public LoginResponse login(LoginRequest request) {
        // Find user by email or phone
        Optional<User> userOpt = findUserByEmailOrPhone(request.getEmailOrPhone());

        if (userOpt.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_USER_NOT_FOUND.getMessage(), request.getEmailOrPhone()));
        }

        User user = userOpt.get();

        // Check if user is active
        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_ACCOUNT_NOT_ACTIVE.getMessage()));
        }

        // Verify password
        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new UnauthorizedException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_INVALID_CREDENTIALS.getMessage()));
        }

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

        // Check if phone already exists
        if (userRepository.existsByPhone(request.getPhone()) || request.getPhone().isEmpty()) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_ALREADY_EXISTS.getMessage(), request.getPhone()));
        }

        // Check if email already exists
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_EMAIL_ALREADY_EXISTS.getMessage(), request.getEmail()));
        }

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
     * Find user by email or phone
     */
    private Optional<User> findUserByEmailOrPhone(String emailOrPhone) {
        // Try to find by email first
        Optional<User> userByEmail = userRepository.findByEmail(emailOrPhone);
        if (userByEmail.isPresent()) {
            return userByEmail;
        }

        // If not found by email, try by phone
        return userRepository.findByPhone(emailOrPhone);
    }

    /**
     * Check if user exists by email or phone
     */
    public boolean userExists(String emailOrPhone) {
        return findUserByEmailOrPhone(emailOrPhone).isPresent();
    }

    /**
     * Check if phone number exists
     */
    public boolean phoneExists(String phone) {
        return userRepository.existsByPhone(phone);
    }

    /**
     * Check if email exists
     */
    public boolean emailExists(String email) {
        return userRepository.existsByEmail(email);
    }

    /**
     * Change current user password
     */
    public boolean changePassword(String oldPassword, String newPassword) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Verify old password
        if (!passwordEncoder.matches(oldPassword, currentUser.getPassword())) {
            throw new UnauthorizedException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_OLD_PASSWORD_INCORRECT.getMessage()));
        }

        // Update password
        currentUser.setPassword(passwordEncoder.encode(newPassword));
        userService.updateUser(currentUser.getId(), currentUser);

        return true;
    }

    /**
     * Validate JWT token
     */
    public boolean validateToken(String token) {
        try {
            return jwtUtil.validateToken(token);
        } catch (Exception e) {
            log.error("Error validating token: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Validate JWT token and return user information if valid
     */
    public LoginResponse validateTokenAndGetUserInfo(String token) {
        try {
            if (!jwtUtil.validateToken(token)) {
                return null;
            }

            // Extract user information from token
            String userId = jwtUtil.extractUserId(token);
            String phone = jwtUtil.extractPhone(token);
            String name = jwtUtil.extractName(token);

            // Verify user still exists and is active
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty() || userOpt.get().getStatus() != UserStatus.ACTIVE) {
                return null;
            }

            User user = userOpt.get();

            return LoginResponse.builder()
                    .token(token)
                    .userId(userId)
                    .name(name)
                    .email(user.getEmail()) // Get email from user repository
                    .phone(phone)
                    .status(user.getStatus())
                    .accountType(user.getAccountType())
                    .message(localizedErrorService
                            .getLocalizedMessage(AuthErrorCode.AUTH_TOKEN_VALID.getMessage()))
                    .build();

        } catch (Exception e) {
            log.error("Error validating token and getting user info: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Validate JWT token from authorization header and return user information if
     * valid
     */
    public LoginResponse validateTokenFromHeader(String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return null;
        }

        String token = authHeader.substring(7);
        return validateTokenAndGetUserInfo(token);
    }

    /**
     * Change current user password and return response
     */
    public AuthResponse changePasswordWithResponse(String oldPassword, String newPassword) {
        changePassword(oldPassword, newPassword);

        return AuthResponse.builder()
                .success(true)
                .message(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_PASSWORD_CHANGED_SUCCESSFULLY.getMessage()))
                .build();
    }
}