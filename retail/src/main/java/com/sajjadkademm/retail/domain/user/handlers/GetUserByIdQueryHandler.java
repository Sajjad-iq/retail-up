package com.sajjadkademm.retail.domain.user.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.user.queries.GetUserByIdQuery;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@RequiredArgsConstructor
public class GetUserByIdQueryHandler implements QueryHandler<GetUserByIdQuery, User> {

    private final UserRepository userRepository;

    @Override
    public User handle(GetUserByIdQuery query) throws Exception {
        log.debug("Handling GetUserByIdQuery for user: {}", query.getUserId());

        return userRepository.findById(query.getUserId())
            .orElseThrow(() -> new NotFoundException("User not found: " + query.getUserId()));
    }

    @Override
    public Class<GetUserByIdQuery> getQueryType() {
        return GetUserByIdQuery.class;
    }
}