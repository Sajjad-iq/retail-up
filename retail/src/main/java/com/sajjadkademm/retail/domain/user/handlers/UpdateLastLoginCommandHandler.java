package com.sajjadkademm.retail.domain.user.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.user.commands.UpdateLastLoginCommand;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateLastLoginCommandHandler implements CommandHandler<UpdateLastLoginCommand, User> {

    private final UserRepository userRepository;

    @Override
    public User handle(UpdateLastLoginCommand command) throws Exception {
        log.debug("Handling UpdateLastLoginCommand for user: {}", command.getUserId());

        User user = userRepository.findById(command.getUserId())
            .orElseThrow(() -> new NotFoundException("User not found: " + command.getUserId()));

        user.setLastLoginAt(command.getLastLoginAt());
        User savedUser = userRepository.save(user);
        
        log.debug("Successfully updated last login for user: {}", savedUser.getEmail());
        
        return savedUser;
    }

    @Override
    public Class<UpdateLastLoginCommand> getCommandType() {
        return UpdateLastLoginCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}