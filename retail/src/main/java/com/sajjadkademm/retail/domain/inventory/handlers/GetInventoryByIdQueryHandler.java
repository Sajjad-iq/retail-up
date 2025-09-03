package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryByIdQuery;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory by ID
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryByIdQueryHandler implements QueryHandler<GetInventoryByIdQuery, Inventory> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;

    @Override
    public Inventory handle(GetInventoryByIdQuery query) throws Exception {
        log.debug("Handling GetInventoryByIdQuery for inventory: {}", query.getInventoryId());

        // Find inventory
        Inventory inventory = inventoryRepository.findById(query.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), query.getInventoryId())));

        // Validate user access
        validationUtils.validateReadAccess(inventory, query.getUserId());
        
        return inventory;
    }

    @Override
    public Class<GetInventoryByIdQuery> getQueryType() {
        return GetInventoryByIdQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetInventoryByIdQuery query) {
        return "inventory-by-id:" + query.getInventoryId() + ":" + query.getUserId();
    }
}