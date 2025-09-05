package com.sajjadkademm.retail.domain.InventoryItem.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.InventoryItem.queries.GetInventoryItemsQuery;
import com.sajjadkademm.retail.application.dto.inventory.PagedResponse;
import com.sajjadkademm.retail.application.dto.inventory.FilterRequest;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;
import com.sajjadkademm.retail.domain.InventoryItem.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.InventoryItem.validation.InventoryItemValidationUtils;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting filtered and paginated inventory items.
 * Pure CQRS implementation using repositories directly with caching capabilities.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryItemsQueryHandler implements QueryHandler<GetInventoryItemsQuery, PagedResponse<InventoryItem>> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;

    @Override
    public PagedResponse<InventoryItem> handle(GetInventoryItemsQuery query) throws Exception {
        log.debug("Handling GetInventoryItemsQuery for inventory: {}", query.getInventoryId());
        
        // Validate user access to inventory
        validationUtils.checkUserAccessToInventory(query.getInventoryId());
        
        // Create pageable with sorting
        Sort.Direction direction = "asc".equalsIgnoreCase(query.getSortDirection()) 
            ? Sort.Direction.ASC : Sort.Direction.DESC;
        Pageable pageable = PageRequest.of(query.getPage(), query.getSize(), 
                                         Sort.by(direction, query.getSortBy()));
        
        // Apply filtering based on FilterRequest
        FilterRequest filter = query.getFilterRequest();
        Page<InventoryItem> pageResult;
        
        if (isComplexFilter(filter)) {
            // Use advanced filtering method
            pageResult = inventoryItemRepository.findWithFilters(
                query.getInventoryId(),
                filter.getCategory(),
                filter.getBrand(),
                filter.getSupplierName(),
                filter.getColor(),
                filter.getSize(),
                filter.getIsActive(),
                filter.getIsPerishable(),
                filter.getMinStock(),
                filter.getMaxStock(),
                filter.getMinCostPrice(),
                filter.getMaxCostPrice(),
                filter.getMinSellingPrice(),
                filter.getMaxSellingPrice(),
                filter.getSearchTerm(),
                pageable
            );
        } else if (filter != null && filter.getSearchTerm() != null) {
            // Use search method
            pageResult = inventoryItemRepository.searchItems(
                query.getInventoryId(),
                filter.getSearchTerm(),
                pageable
            );
        } else {
            // Default: get all active items
            pageResult = inventoryItemRepository.findByInventoryIdAndIsActiveTrue(
                query.getInventoryId(),
                pageable
            );
        }
        
        // Convert to PagedResponse
        return PagedResponse.<InventoryItem>builder()
            .content(pageResult.getContent())
            .page(pageResult.getNumber())
            .size(pageResult.getSize())
            .totalElements(pageResult.getTotalElements())
            .totalPages(pageResult.getTotalPages())
            .first(pageResult.isFirst())
            .last(pageResult.isLast())
            .hasNext(pageResult.hasNext())
            .hasPrevious(pageResult.hasPrevious())
            .numberOfElements(pageResult.getNumberOfElements())
            .empty(pageResult.isEmpty())
            .build();
    }
    
    private boolean isComplexFilter(FilterRequest filter) {
        if (filter == null) return false;
        
        return filter.getCategory() != null ||
               filter.getBrand() != null ||
               filter.getSupplierName() != null ||
               filter.getColor() != null ||
               filter.getSize() != null ||
               filter.getIsActive() != null ||
               filter.getIsPerishable() != null ||
               filter.getMinStock() != null ||
               filter.getMaxStock() != null ||
               filter.getMinCostPrice() != null ||
               filter.getMaxCostPrice() != null ||
               filter.getMinSellingPrice() != null ||
               filter.getMaxSellingPrice() != null;
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