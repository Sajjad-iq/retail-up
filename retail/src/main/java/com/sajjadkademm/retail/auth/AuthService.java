package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.UserStatus;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    /**
     * Authenticate user with email/phone and password
     */
    public LoginResponse login(LoginRequest request) {
        log.info("Attempting login for user: {}", request.getEmailOrPhone());

        // Find user by email or phone
        Optional<User> userOpt = findUserByEmailOrPhone(request.getEmailOrPhone());

        if (userOpt.isEmpty()) {
            return LoginResponse.builder()
                    .message("User not found")
                    .build();
        }

        User user = userOpt.get();

        // Check if user is active
        if (user.getStatus() != UserStatus.ACTIVE) {
            return LoginResponse.builder()
                    .message("Account is not active")
                    .build();
        }

        // Verify password
        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            return LoginResponse.builder()
                    .message("Invalid credentials")
                    .build();
        }

        // Update last login time
        user.setLastLoginAt(LocalDateTime.now());
        userRepository.save(user);

        // Generate token (simple UUID for now, can be replaced with JWT)
        String token = UUID.randomUUID().toString();

        log.info("Login successful for user: {}", user.getEmail());

        return LoginResponse.builder()
                .token(token)
                .userId(user.getId())
                .name(user.getName())
                .email(user.getEmail())
                .phone(user.getPhone())
                .message("Login successful")
                .build();
    }

    /**
     * Register new user
     */
    public LoginResponse register(RegisterRequest request) {
        log.info("Attempting registration for user: {}", request.getEmail());

        // Check if email already exists
        if (request.getEmail() != null && userRepository.existsByEmail(request.getEmail())) {
            return LoginResponse.builder()
                    .message("Email already exists")
                    .build();
        }

        // Check if phone already exists
        if (userRepository.existsByPhone(request.getPhone())) {
            return LoginResponse.builder()
                    .message("Phone number already exists")
                    .build();
        }

        // Create new user
        User newUser = User.builder()
                .name(request.getName())
                .email(request.getEmail())
                .phone(request.getPhone())
                .password(passwordEncoder.encode(request.getPassword()))
                .status(request.getStatus())
                .build();

        // Save user using UserService
        User savedUser = userService.createUser(newUser);

        // Generate token
        String token = UUID.randomUUID().toString();

        log.info("Registration successful for user: {}", savedUser.getEmail());

        return LoginResponse.builder()
                .token(token)
                .userId(savedUser.getId())
                .name(savedUser.getName())
                .email(savedUser.getEmail())
                .phone(savedUser.getPhone())
                .message("Registration successful")
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
     * Change user password
     */
    public boolean changePassword(String userId, String oldPassword, String newPassword) {
        try {
            User user = userService.getUserById(Long.parseLong(userId));
            if (user == null) {
                return false;
            }

            // Verify old password
            if (!passwordEncoder.matches(oldPassword, user.getPassword())) {
                return false;
            }

            // Update password
            user.setPassword(passwordEncoder.encode(newPassword));
            userService.updateUser(Long.parseLong(userId), user);

            return true;
        } catch (Exception e) {
            log.error("Error changing password for user: {}", userId, e);
            return false;
        }
    }
}