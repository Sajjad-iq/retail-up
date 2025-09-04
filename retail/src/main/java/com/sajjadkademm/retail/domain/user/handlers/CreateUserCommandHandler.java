package com.sajjadkademm.retail.domain.user.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.commands.CreateUserCommand;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.domain.shared.validation.PhoneValidator;
import com.sajjadkademm.retail.domain.shared.validation.EmailValidator;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class CreateUserCommandHandler implements CommandHandler<CreateUserCommand, User> {

    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;

    @Override
    public User handle(CreateUserCommand command) throws Exception {
        log.debug("Handling CreateUserCommand for user: {}", command.getUser().getEmail());

        User newUser = command.getUser();
        
        // Validate the new user data
        if (newUser.getName() == null || newUser.getName().trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INVALID_USER_DATA.getMessage()));
        }

        // Validate phone format and uniqueness
        phoneValidator.validatePhoneFormatAndUniqueness(newUser.getPhone(),
                (phone) -> userRepository.existsByPhone(newUser.getPhone()));

        // Validate email format and uniqueness if provided
        if (newUser.getEmail() != null) {
            emailValidator.validateEmailFormatAndUniqueness(newUser.getEmail(),
                    (email) -> userRepository.existsByEmail(newUser.getEmail()));
        }
        
        // Set default values if not provided
        if (newUser.getStatus() == null) {
            newUser.setStatus(UserStatus.ACTIVE);
        }
        if (newUser.getAccountType() == null) {
            newUser.setAccountType(AccountType.USER); // Match original UserService behavior
        }

        User savedUser = userRepository.save(newUser);
        
        log.info("Successfully created user: {} ({})", savedUser.getName(), savedUser.getEmail());
        
        return savedUser;
    }

    @Override
    public Class<CreateUserCommand> getCommandType() {
        return CreateUserCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}