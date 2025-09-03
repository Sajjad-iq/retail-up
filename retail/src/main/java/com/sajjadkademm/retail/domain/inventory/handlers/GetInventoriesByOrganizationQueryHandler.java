package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoriesByOrganizationQuery;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Query handler for getting inventories by organization
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoriesByOrganizationQueryHandler implements QueryHandler<GetInventoriesByOrganizationQuery, List<Inventory>> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;

    @Override
    public List<Inventory> handle(GetInventoriesByOrganizationQuery query) throws Exception {
        log.debug("Handling GetInventoriesByOrganizationQuery for organization: {}", query.getOrganizationId());

        // Validate user access to organization
        validationUtils.validateOrganizationAccess(query.getOrganizationId(), query.getUserId());
        
        // Get inventories based on activeOnly flag
        List<Inventory> inventories;
        if (Boolean.TRUE.equals(query.getActiveOnly())) {
            inventories = inventoryRepository.findByOrganizationIdAndIsActiveTrue(query.getOrganizationId());
        } else {
            inventories = inventoryRepository.findByOrganizationId(query.getOrganizationId());
        }
        
        return inventories;
    }

    @Override
    public Class<GetInventoriesByOrganizationQuery> getQueryType() {
        return GetInventoriesByOrganizationQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetInventoriesByOrganizationQuery query) {
        return "inventories-by-org:" + query.getOrganizationId() + ":" + 
               query.getActiveOnly() + ":" + query.getUserId();
    }
}