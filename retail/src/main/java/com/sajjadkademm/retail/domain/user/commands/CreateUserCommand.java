package com.sajjadkademm.retail.domain.user.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class CreateUserCommand implements Command<User> {
    private final User user;
    private final String requestId;

    @Override
    public String getUserId() {
        return null; // No user context for creation
    }

    public String getRequestId() {
        return requestId;
    }
}