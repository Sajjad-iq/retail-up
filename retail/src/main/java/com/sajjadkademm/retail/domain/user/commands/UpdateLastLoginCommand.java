package com.sajjadkademm.retail.domain.user.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Getter;
import java.time.LocalDateTime;

@Getter
@Builder
public class UpdateLastLoginCommand implements Command<User> {
    private final String userId;
    private final LocalDateTime lastLoginAt;
    private final String requestId;

    @Override
    public String getUserId() {
        return userId;
    }

    public String getRequestId() {
        return requestId;
    }
}