package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemByIdQuery;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemValidationUtils;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryItemErrorCode;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory item by ID.
 * Pure CQRS implementation using repositories directly with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemByIdQueryHandler implements QueryHandler<GetInventoryItemByIdQuery, InventoryItem> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;

    @Override
    public InventoryItem handle(GetInventoryItemByIdQuery query) throws Exception {
        log.debug("Handling GetInventoryItemByIdQuery for item: {}", query.getItemId());
        
        // Find inventory item
        InventoryItem item = inventoryItemRepository.findById(query.getItemId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage(), query.getItemId())));

        // Validate user access
        validationUtils.checkUserAccessToInventoryItem(item);
        
        return item;
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