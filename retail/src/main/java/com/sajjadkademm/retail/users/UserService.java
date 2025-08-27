package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public UserService(UserRepository userRepository, LocalizedErrorService localizedErrorService) {
        this.userRepository = userRepository;
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Get current authenticated user's profile
     * This method is secure and only returns the current user's information
     */
    public User getCurrentUserProfile() {
        return SecurityUtils.getCurrentUser();
    }

    /**
     * Update current authenticated user's profile
     * This method is secure and only allows users to update their own profile
     */
    public User updateCurrentUserProfile(User userDetails) {
        User currentUser = SecurityUtils.getCurrentUser();

        // Only allow updating own profile
        if (!currentUser.getId().equals(userDetails.getId())) {
            throw new UnauthorizedException("Users can only update their own profile");
        }

        // Update allowed fields (exclude sensitive fields like password, status,
        // accountType)
        currentUser.setName(userDetails.getName());
        currentUser.setPhone(userDetails.getPhone());
        // Note: Password changes should use the dedicated changePassword method in
        // AuthService

        return userRepository.save(currentUser);
    }

    // Admin-only methods - these should only be accessible by admin users
    // TODO: Add role-based access control when role system is implemented

    /**
     * Get all users (admin only)
     * TODO: Add admin role check
     */
    public List<User> getAllUsers() {
        // TODO: Add admin role check
        return userRepository.findAll();
    }

    /**
     * Get user by ID (admin only)
     * TODO: Add admin role check
     */
    public User getUserById(String id) {
        // TODO: Add admin role check
        Optional<User> user = userRepository.findById(id);
        if (user.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }
        return user.get();
    }

    /**
     * Create user (admin only)
     * TODO: Add admin role check
     */
    public User createUser(User user) {
        // TODO: Add admin role check
        return userRepository.save(user);
    }

    /**
     * Update user (admin only)
     * TODO: Add admin role check
     */
    public User updateUser(String id, User userDetails) {
        // TODO: Add admin role check
        Optional<User> optionalUser = userRepository.findById(id);
        if (optionalUser.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        User user = optionalUser.get();
        user.setName(userDetails.getName());
        user.setPassword(userDetails.getPassword());
        user.setPhone(userDetails.getPhone());
        user.setStatus(userDetails.getStatus());
        return userRepository.save(user);
    }

    /**
     * Delete user (admin only)
     * TODO: Add admin role check
     */
    public boolean deleteUser(String id) {
        // TODO: Add admin role check
        if (!userRepository.existsById(id)) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }
        userRepository.deleteById(id);
        return true;
    }
}
