package com.sajjadkademm.retail.domain.organization.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.organization.queries.GetUserOrganizationsQuery;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.auth.validation.UserValidator;

import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Component;
import java.util.List;

@Component
@RequiredArgsConstructor
public class GetUserOrganizationsQueryHandler implements QueryHandler<GetUserOrganizationsQuery, List<Organization>> {

    private final OrganizationRepository organizationRepository;
    private final UserValidator userValidator;

    @Override
    @Cacheable(value = "userOrganizations", key = "#query.userId")
    public List<Organization> handle(GetUserOrganizationsQuery query) {
        userValidator.validateUserActive(query.getUserId());
        
        return organizationRepository.findByCreatedById(query.getUserId());
    }

    @Override
    public Class<GetUserOrganizationsQuery> getQueryType() {
        return GetUserOrganizationsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }
}