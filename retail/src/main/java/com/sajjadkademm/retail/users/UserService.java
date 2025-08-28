package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.shared.enums.AccountType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;

    @Autowired
    public UserService(UserRepository userRepository, LocalizedErrorService localizedErrorService,
            UserValidator userValidator) {
        this.userRepository = userRepository;
        this.localizedErrorService = localizedErrorService;
        this.userValidator = userValidator;
    }

    /**
     * Get current authenticated user's profile
     * This method is secure and only returns the current user's information
     */
    public User getCurrentUserProfile() {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActive(currentUser);
        return currentUser;
    }

    /**
     * Update current authenticated user's profile
     * This method is secure and only allows users to update their own profile
     */
    public User updateCurrentUserProfile(User userDetails) {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActive(currentUser);

        // Only allow updating own profile
        if (!currentUser.getId().equals(userDetails.getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INSUFFICIENT_PERMISSIONS.getMessage()));
        }

        // Update allowed fields (exclude sensitive fields like password, status,
        // accountType)
        currentUser.setName(userDetails.getName());
        currentUser.setPhone(userDetails.getPhone());
        // Note: Password changes should use the dedicated changePassword method in
        // AuthService

        return userRepository.save(currentUser);
    }

    // USER-only methods - these should only be accessible by USER users

    /**
     * Get all users (USER only)
     */
    public List<User> getAllUsers() {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);
        return userRepository.findAll();
    }

    /**
     * Get user by ID (USER only)
     */
    public User getUserById(String id) {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);

        Optional<User> user = userRepository.findById(id);
        if (user.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        User targetUser = user.get();
        userValidator.assertUserIsActive(targetUser);
        return targetUser;
    }

    /**
     * Create user (USER only)
     */
    public User createUser(User user) {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);

        // Validate the new user data
        if (user.getName() == null || user.getName().trim().isEmpty()) {
            throw new IllegalArgumentException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        return userRepository.save(user);
    }

    /**
     * Update user (USER only)
     */
    public User updateUser(String id, User userDetails) {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);

        Optional<User> optionalUser = userRepository.findById(id);
        if (optionalUser.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        User user = optionalUser.get();
        userValidator.assertUserIsActive(user);

        // Validate the update data
        if (userDetails.getName() == null || userDetails.getName().trim().isEmpty()) {
            throw new IllegalArgumentException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        user.setName(userDetails.getName());
        user.setPhone(userDetails.getPhone());
        user.setStatus(userDetails.getStatus());
        // Note: Password updates should be handled separately with proper validation

        return userRepository.save(user);
    }

    /**
     * Delete user (USER only)
     */
    public boolean deleteUser(String id) {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);

        // Prevent USER from deleting themselves
        if (currentUser.getId().equals(id)) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.CANNOT_DELETE_SELF.getMessage()));
        }

        if (!userRepository.existsById(id)) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        userRepository.deleteById(id);
        return true;
    }
}
