package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.queries.UserExistsQuery;
import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.auth.validation.AuthValidator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Query handler for checking if a user exists by email or phone
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UserExistsQueryHandler implements QueryHandler<UserExistsQuery, Boolean> {

    private final AuthValidator authValidator;

    @Override
    public Boolean handle(UserExistsQuery query) throws Exception {
        log.debug("Processing user exists query for: {}", query.getEmailOrPhone());

        boolean exists = authValidator.userExists(query.getEmailOrPhone());
        
        log.debug("User exists check for {}: {}", query.getEmailOrPhone(), exists);
        
        return exists;
    }

    @Override
    public Class<UserExistsQuery> getQueryType() {
        return UserExistsQuery.class;
    }
}