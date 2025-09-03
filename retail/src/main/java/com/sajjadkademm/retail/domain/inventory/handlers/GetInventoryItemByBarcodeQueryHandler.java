package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemByBarcodeQuery;
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
 * Query handler for getting inventory item by barcode.
 * Pure CQRS implementation using repositories directly with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemByBarcodeQueryHandler implements QueryHandler<GetInventoryItemByBarcodeQuery, InventoryItem> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;

    @Override
    public InventoryItem handle(GetInventoryItemByBarcodeQuery query) throws Exception {
        log.debug("Handling GetInventoryItemByBarcodeQuery for barcode: {}", query.getBarcode());
        
        // Validate user access to inventory first
        validationUtils.checkUserAccessToInventory(query.getInventoryId());
        
        // Find inventory item by barcode
        InventoryItem item = inventoryItemRepository.findByBarcodeAndInventoryId(query.getBarcode(), query.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage(), 
                                          query.getBarcode(), query.getInventoryId())));
        
        return item;
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