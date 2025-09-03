package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemByBarcodeQuery;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory item by barcode.
 * Delegates to existing InventoryItemService with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemByBarcodeQueryHandler implements QueryHandler<GetInventoryItemByBarcodeQuery, InventoryItem> {

    private final InventoryItemService inventoryItemService;

    @Override
    public InventoryItem handle(GetInventoryItemByBarcodeQuery query) throws Exception {
        log.debug("Handling GetInventoryItemByBarcodeQuery for barcode: {}", query.getBarcode());
        
        // Delegate to existing service - maintains all existing access control
        return inventoryItemService.getInventoryItemByBarcode(query.getBarcode(), query.getInventoryId());
    }

    @Override
    public Class<GetInventoryItemByBarcodeQuery> getQueryType() {
        return GetInventoryItemByBarcodeQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true; // Barcode lookups can be cached
    }

    @Override
    public String getCacheKey(GetInventoryItemByBarcodeQuery query) {
        // Cache key includes inventory ID and user ID for security
        return "inventory-item-barcode:" + query.getBarcode() + ":" + 
               query.getInventoryId() + ":" + query.getUserId();
    }
}