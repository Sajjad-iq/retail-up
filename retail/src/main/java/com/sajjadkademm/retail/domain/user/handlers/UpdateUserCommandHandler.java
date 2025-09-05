package com.sajjadkademm.retail.domain.user.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.commands.UpdateUserCommand;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;
import com.sajjadkademm.retail.domain.shared.validation.PhoneValidator;
import com.sajjadkademm.retail.domain.shared.validation.EmailValidator;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateUserCommandHandler implements CommandHandler<UpdateUserCommand, User> {

    private final UserRepository userRepository;
    private final UserValidator userValidator;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;

    @Override
    public User handle(UpdateUserCommand command) throws Exception {
        log.debug("Handling UpdateUserCommand for user: {}", command.getUserId());

        // Validate user is active using UserValidator (matches original behavior)
        User existingUser = userValidator.validateUserActive(command.getUserId());

        User updatedData = command.getUser();
        
        // Validate and update fields with proper validation (matching original UserService logic)
        if (updatedData.getName() != null && !updatedData.getName().trim().isEmpty()
                && !updatedData.getName().equals(existingUser.getName())) {
            existingUser.setName(updatedData.getName());
        }

        if (updatedData.getEmail() != null && !updatedData.getEmail().equals(existingUser.getEmail())) {
            emailValidator.validateEmailFormatAndUniqueness(updatedData.getEmail(),
                    (email) -> userRepository.existsByEmail(updatedData.getEmail()));
            existingUser.setEmail(updatedData.getEmail());
        }

        if (updatedData.getPhone() != null && !updatedData.getPhone().equals(existingUser.getPhone())) {
            phoneValidator.validatePhoneFormatAndUniqueness(updatedData.getPhone(),
                    (phone) -> userRepository.existsByPhone(updatedData.getPhone()));
            existingUser.setPhone(updatedData.getPhone());
        }


        if (updatedData.getStatus() != null) {
            existingUser.setStatus(updatedData.getStatus());
        }
        if (updatedData.getAccountType() != null) {
            existingUser.setAccountType(updatedData.getAccountType());
        }
        if (updatedData.getLastLoginAt() != null) {
            existingUser.setLastLoginAt(updatedData.getLastLoginAt());
        }

        User savedUser = userRepository.save(existingUser);
        
        log.info("Successfully updated user: {} ({})", savedUser.getName(), savedUser.getEmail());
        
        return savedUser;
    }

    @Override
    public Class<UpdateUserCommand> getCommandType() {
        return UpdateUserCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}