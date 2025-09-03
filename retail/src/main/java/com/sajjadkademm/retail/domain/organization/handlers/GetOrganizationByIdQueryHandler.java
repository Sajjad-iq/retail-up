package com.sajjadkademm.retail.domain.organization.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.organization.queries.GetOrganizationByIdQuery;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidationUtils;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;

import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class GetOrganizationByIdQueryHandler implements QueryHandler<GetOrganizationByIdQuery, Organization> {

    private final OrganizationRepository organizationRepository;
    private final OrganizationValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;

    @Override
    @Cacheable(value = "organizations", key = "#query.organizationId")
    public Organization handle(GetOrganizationByIdQuery query) {
        Organization organization = organizationRepository.findById(query.getOrganizationId())
                .orElseThrow(() -> new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));

        validationUtils.validateUserAccess(organization, query.getUserId());
        
        return organization;
    }

    @Override
    public Class<GetOrganizationByIdQuery> getQueryType() {
        return GetOrganizationByIdQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }
}