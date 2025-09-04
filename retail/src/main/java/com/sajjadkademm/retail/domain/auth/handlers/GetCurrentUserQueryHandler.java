package com.sajjadkademm.retail.domain.auth.handlers;

import com.sajjadkademm.retail.domain.auth.queries.GetCurrentUserQuery;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Query handler for getting current user profile
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetCurrentUserQueryHandler implements QueryHandler<GetCurrentUserQuery, User> {

    private final UserRepository userRepository;

    @Override
    public User handle(GetCurrentUserQuery query) throws Exception {
        log.debug("Processing get current user query for user: {}", query.getUserId());

        // Directly use UserRepository to avoid circular dependency
        User user = userRepository.findById(query.getUserId())
            .orElseThrow(() -> new NotFoundException("User not found: " + query.getUserId()));
        
        log.debug("Retrieved current user profile: {} ({})", user.getName(), user.getEmail());
        
        return user;
    }

    @Override
    public Class<GetCurrentUserQuery> getQueryType() {
        return GetCurrentUserQuery.class;
    }
}