package com.sajjadkademm.retail.domain.organization.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.organization.queries.SearchUserOrganizationsQuery;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;

import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Component;
import java.util.List;

@Component
@RequiredArgsConstructor
public class SearchUserOrganizationsQueryHandler implements QueryHandler<SearchUserOrganizationsQuery, List<Organization>> {

    private final OrganizationRepository organizationRepository;
    private final UserValidator userValidator;

    @Override
    @Cacheable(value = "organizationSearch", key = "#query.userId + '_' + #query.searchTerm")
    public List<Organization> handle(SearchUserOrganizationsQuery query) {
        userValidator.validateUserActive(query.getUserId());
        
        return organizationRepository.searchByUserIdAndTerm(query.getUserId(), query.getSearchTerm());
    }

    @Override
    public Class<SearchUserOrganizationsQuery> getQueryType() {
        return SearchUserOrganizationsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }
}