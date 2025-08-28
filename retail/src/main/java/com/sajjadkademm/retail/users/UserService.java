package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.validators.PhoneValidator;
import com.sajjadkademm.retail.shared.validators.EmailValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;

    @Autowired
    public UserService(UserRepository userRepository, LocalizedErrorService localizedErrorService,
            UserValidator userValidator, PhoneValidator phoneValidator, EmailValidator emailValidator) {
        this.userRepository = userRepository;
        this.localizedErrorService = localizedErrorService;
        this.userValidator = userValidator;
        this.phoneValidator = phoneValidator;
        this.emailValidator = emailValidator;

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
     * Get user by ID (USER only)
     */
    public User getUserById(String id) {
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
        // Validate the new user data
        if (user.getName() == null || user.getName().trim().isEmpty()) {
            throw new IllegalArgumentException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        phoneValidator.validatePhoneFormatAndUniqueness(user.getPhone(),
                (phone) -> userRepository.existsByPhone(phone));

        emailValidator.validateEmailFormatAndUniqueness(user.getEmail(),
                (email) -> userRepository.existsByEmail(email));

        return userRepository.save(user);
    }

    /**
     * Update user (USER only)
     */
    public User updateUser(String id, User userDetails) {
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

        if(userDetails.getEmail() != null){
            emailValidator.validateEmailFormatAndUniqueness(user.getEmail(),
                    (email) -> userRepository.existsByEmail(user.getEmail()));
            user.setEmail(userDetails.getEmail());
        }


        user.setName(userDetails.getName());
        user.setPhone(userDetails.getPhone());
        user.setStatus(userDetails.getStatus());

        return userRepository.save(user);
    }

}
