package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryCountByOrganizationQuery;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory count by organization
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryCountByOrganizationQueryHandler implements QueryHandler<GetInventoryCountByOrganizationQuery, Long> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;

    @Override
    public Long handle(GetInventoryCountByOrganizationQuery query) throws Exception {
        log.debug("Handling GetInventoryCountByOrganizationQuery for organization: {}", query.getOrganizationId());

        // Validate user access to organization
        validationUtils.validateOrganizationAccess(query.getOrganizationId(), query.getUserId());
        
        // Get count of inventories in organization
        Long count = inventoryRepository.countByOrganizationId(query.getOrganizationId());
        
        return count;
    }

    @Override
    public Class<GetInventoryCountByOrganizationQuery> getQueryType() {
        return GetInventoryCountByOrganizationQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetInventoryCountByOrganizationQuery query) {
        return "inventory-count-by-org:" + query.getOrganizationId() + ":" + query.getUserId();
    }
}