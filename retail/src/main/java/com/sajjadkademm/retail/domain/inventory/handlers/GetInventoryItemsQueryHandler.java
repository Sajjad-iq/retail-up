package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryItemsQuery;
import com.sajjadkademm.retail.application.dto.inventory.PagedResponse;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting filtered and paginated inventory items.
 * Delegates to existing InventoryItemService with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemsQueryHandler implements QueryHandler<GetInventoryItemsQuery, PagedResponse<InventoryItem>> {

    private final InventoryItemService inventoryItemService;

    @Override
    public PagedResponse<InventoryItem> handle(GetInventoryItemsQuery query) throws Exception {
        log.debug("Handling GetInventoryItemsQuery for inventory: {}", query.getInventoryId());
        
        // Delegate to existing service - maintains all existing filtering and pagination logic
        return inventoryItemService.filterItemsPaginated(
            query.getInventoryId(),
            query.getFilterRequest(),
            query.getPage(),
            query.getSize(),
            query.getSortBy(),
            query.getSortDirection()
        );
    }

    @Override
    public Class<GetInventoryItemsQuery> getQueryType() {
        return GetInventoryItemsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        // Enable caching for this query type - specific caching logic is in getCacheKey
        return true;
    }

    @Override
    public String getCacheKey(GetInventoryItemsQuery query) {
        if (query.getPage() == 0 && 
            (query.getFilterRequest() == null || isEmptyFilter(query.getFilterRequest()))) {
            // Cache key for default listing
            return "inventory-items-list:" + query.getInventoryId() + ":" + 
                   query.getSize() + ":" + query.getUserId();
        }
        return null;
    }

    private boolean isEmptyFilter(com.sajjadkademm.retail.application.dto.inventory.FilterRequest filter) {
        if (filter == null) return true;
        
        return filter.getCategory() == null &&
               filter.getBrand() == null &&
               filter.getSupplierName() == null &&
               filter.getSearchTerm() == null &&
               filter.getIsActive() == null &&
               filter.getIsPerishable() == null &&
               filter.getLowStock() == null &&
               filter.getOutOfStock() == null &&
               filter.getHasDiscount() == null;
    }
}