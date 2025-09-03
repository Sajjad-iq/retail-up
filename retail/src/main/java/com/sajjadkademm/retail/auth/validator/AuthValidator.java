package com.sajjadkademm.retail.auth.validator;

import com.sajjadkademm.retail.shared.common.exceptions.ConflictException;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.shared.enums.UserStatus;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * Centralized validation helpers for Authentication domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class AuthValidator {

    private final LocalizedErrorService localizedErrorService;
    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder;

    @Autowired
    public AuthValidator(LocalizedErrorService localizedErrorService,
            UserRepository userRepository,
            BCryptPasswordEncoder passwordEncoder) {
        this.localizedErrorService = localizedErrorService;
        this.userRepository = userRepository;
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * Validates user credentials during login.
     * Checks if user exists and password matches.
     *
     * @param emailOrPhone the email or phone to search for
     * @param password     the password to verify
     * @return the validated user
     * @throws NotFoundException     when user is not found
     * @throws UnauthorizedException when password is incorrect
     */
    public User validateLoginCredentials(String emailOrPhone, String password) {
        // Find user by email or phone
        Optional<User> userOpt = findUserByEmailOrPhone(emailOrPhone);

        if (userOpt.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_USER_NOT_FOUND.getMessage(), emailOrPhone));
        }

        User user = userOpt.get();

        // Verify password
        if (!passwordEncoder.matches(password, user.getPassword())) {
            throw new BadRequestException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_INVALID_CREDENTIALS.getMessage()));
        }

        return user;
    }

    /**
     * Validates old password during password change.
     * Checks if the old password matches the current user's password.
     *
     * @param oldPassword the old password to verify
     * @param currentUser the current authenticated user
     * @throws UnauthorizedException when old password is incorrect
     */
    public void validateOldPassword(String oldPassword, User currentUser) {
        if (!passwordEncoder.matches(oldPassword, currentUser.getPassword())) {
            throw new BadRequestException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_OLD_PASSWORD_INCORRECT.getMessage()));
        }
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
}
