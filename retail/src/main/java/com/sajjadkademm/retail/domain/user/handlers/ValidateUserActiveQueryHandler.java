package com.sajjadkademm.retail.domain.user.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.user.queries.ValidateUserActiveQuery;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class ValidateUserActiveQueryHandler implements QueryHandler<ValidateUserActiveQuery, User> {

    private final UserValidator userValidator;

    @Override
    public User handle(ValidateUserActiveQuery query) throws Exception {
        log.debug("Handling ValidateUserActiveQuery for user: {}", query.getUserId());

        return userValidator.validateUserActive(query.getUserId());
    }

    @Override
    public Class<ValidateUserActiveQuery> getQueryType() {
        return ValidateUserActiveQuery.class;
    }
}