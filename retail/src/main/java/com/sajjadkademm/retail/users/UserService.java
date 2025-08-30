package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.shared.validators.PhoneValidator;
import com.sajjadkademm.retail.shared.validators.EmailValidator;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class UserService {
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;

    /**
     * Get current authenticated user's profile
     * This method is secure and only returns the current user's information
     */
    public User getCurrentUserProfile() {
        User currentUser = SecurityUtils.getCurrentUser();
        userValidator.assertUserIsHasActiveStatus(currentUser);
        return currentUser;
    }

    /**
     * Get user by ID (USER only)
     */
    public User getUserById(String id) {
        Optional<User> user = userRepository.findById(id);
        if (user.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        User targetUser = user.get();
        userValidator.assertUserIsHasActiveStatus(targetUser);
        return targetUser;
    }

    /**
     * Create user (USER only)
     */
    @Transactional(rollbackFor = Exception.class)
    public User createUser(User user) {
        // Validate the new user data
        if (user.getName() == null || user.getName().trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        phoneValidator.validatePhoneFormatAndUniqueness(user.getPhone(),
                (phone) -> userRepository.existsByPhone(user.getPhone()));

        if (user.getEmail() != null) {
            emailValidator.validateEmailFormatAndUniqueness(user.getEmail(),
                    (email) -> userRepository.existsByEmail(user.getEmail()));
        }

        return userRepository.save(user);
    }

    /**
     * Update user (USER only)
     */
    @Transactional(rollbackFor = Exception.class)
    public User updateUser(String id, User userDetails) {

        User user = userValidator.validateUserActive(id);

        // Validate the update data
        if (userDetails.getName() == null || userDetails.getName().trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        if (userDetails.getEmail() != null && !userDetails.getEmail().equals(user.getEmail())) {
            emailValidator.validateEmailFormatAndUniqueness(user.getEmail(),
                    (email) -> userRepository.existsByEmail(user.getEmail()));
            user.setEmail(userDetails.getEmail());
        }

        if (userDetails.getPhone() != null && !userDetails.getPhone().equals(user.getPhone())) {
            phoneValidator.validatePhoneFormatAndUniqueness(user.getPhone(),
                    (phone) -> userRepository.existsByPhone(user.getPhone()));
            user.setPhone(userDetails.getPhone());
        }

        user.setName(userDetails.getName());

        return userRepository.save(user);
    }

}
