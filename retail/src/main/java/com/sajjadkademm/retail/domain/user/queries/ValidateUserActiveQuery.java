package com.sajjadkademm.retail.domain.user.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class ValidateUserActiveQuery implements Query<User> {
    private final String userId;
    private final String requestId;

    @Override
    public String getUserId() {
        return userId;
    }

    public String getRequestId() {
        return requestId;
    }
}