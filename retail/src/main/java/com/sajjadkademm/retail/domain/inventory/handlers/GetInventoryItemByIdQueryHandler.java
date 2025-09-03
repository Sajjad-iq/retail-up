package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemByIdQuery;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory item by ID.
 * Delegates to existing InventoryItemService with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemByIdQueryHandler implements QueryHandler<GetInventoryItemByIdQuery, InventoryItem> {

    private final InventoryItemService inventoryItemService;

    @Override
    public InventoryItem handle(GetInventoryItemByIdQuery query) throws Exception {
        log.debug("Handling GetInventoryItemByIdQuery for item: {}", query.getItemId());
        
        // Delegate to existing service - maintains all existing access control
        return inventoryItemService.getInventoryItemById(query.getItemId());
    }

    @Override
    public Class<GetInventoryItemByIdQuery> getQueryType() {
        return GetInventoryItemByIdQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true; // Single item queries can be cached
    }

    @Override
    public String getCacheKey(GetInventoryItemByIdQuery query) {
        // Cache key includes user ID for security
        return "inventory-item:" + query.getItemId() + ":" + query.getUserId();
    }
}