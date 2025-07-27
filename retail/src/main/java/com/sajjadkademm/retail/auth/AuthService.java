package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
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

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final UserService userService;
    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
    private final JwtUtil jwtUtil;

    /**
     * Authenticate user with email/phone and password
     */
    public LoginResponse login(LoginRequest request) {
        log.info("Attempting login for user: {}", request.getEmailOrPhone());

        // Find user by email or phone
        Optional<User> userOpt = findUserByEmailOrPhone(request.getEmailOrPhone());

        if (userOpt.isEmpty()) {
            throw new NotFoundException("User not found with identifier: " + request.getEmailOrPhone());
        }

        User user = userOpt.get();

        // Check if user is active
        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException("Account is not active");
        }

        // Verify password
        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new UnauthorizedException("Invalid credentials");
        }

        // Update last login time
        user.setLastLoginAt(LocalDateTime.now());
        userRepository.save(user);

        // Generate JWT token
        String token = jwtUtil.generateToken(user.getId(), user.getEmail(), user.getName());

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
            throw new ConflictException("Email already exists: " + request.getEmail());
        }

        // Check if phone already exists
        if (userRepository.existsByPhone(request.getPhone())) {
            throw new ConflictException("Phone number already exists: " + request.getPhone());
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

        // Generate JWT token
        String token = jwtUtil.generateToken(savedUser.getId(), savedUser.getEmail(), savedUser.getName());

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
        User user = userService.getUserById(userId);
        if (user == null) {
            throw new NotFoundException("User not found with id: " + userId);
        }

        // Verify old password
        if (!passwordEncoder.matches(oldPassword, user.getPassword())) {
            throw new UnauthorizedException("Old password is incorrect");
        }

        // Update password
        user.setPassword(passwordEncoder.encode(newPassword));
        userService.updateUser(userId, user);

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
}