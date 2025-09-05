package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemCountQuery;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemValidationUtils;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory item counts.
 * Pure CQRS implementation using repositories directly.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemCountQueryHandler implements QueryHandler<GetInventoryItemCountQuery, Long> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;

    @Override
    public Long handle(GetInventoryItemCountQuery query) throws Exception {
        log.debug("Handling GetInventoryItemCountQuery for inventory: {}", query.getInventoryId());
        
        // Validate user access to inventory
        validationUtils.checkUserAccessToInventory(query.getInventoryId());
        
        // Get count based on activeOnly flag
        if (Boolean.TRUE.equals(query.getActiveOnly())) {
            return inventoryItemRepository.countByInventoryIdAndIsActiveTrue(query.getInventoryId());
        } else {
            return inventoryItemRepository.countByInventoryId(query.getInventoryId());
        }
    }

    @Override
    public Class<GetInventoryItemCountQuery> getQueryType() {
        return GetInventoryItemCountQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true; // Count operations can be cached
    }

    @Override
    public String getCacheKey(GetInventoryItemCountQuery query) {
        return "inventory-item-count:" + query.getInventoryId() + ":" + 
               query.getActiveOnly() + ":" + query.getUserId();
    }
}