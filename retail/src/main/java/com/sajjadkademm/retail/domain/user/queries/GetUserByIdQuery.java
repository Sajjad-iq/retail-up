package com.sajjadkademm.retail.domain.user.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class GetUserByIdQuery implements Query<User> {
    private final String userId;
    private final String requestingUserId;
    private final String requestId;

    @Override
    public String getUserId() {
        return requestingUserId;
    }

    public String getRequestId() {
        return requestId;
    }
}