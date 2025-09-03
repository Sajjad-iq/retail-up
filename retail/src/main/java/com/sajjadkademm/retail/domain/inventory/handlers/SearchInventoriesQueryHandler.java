package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.SearchInventoriesQuery;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Query handler for searching inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class SearchInventoriesQueryHandler implements QueryHandler<SearchInventoriesQuery, List<Inventory>> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;

    @Override
    public List<Inventory> handle(SearchInventoriesQuery query) throws Exception {
        log.debug("Handling SearchInventoriesQuery for organization: {} with term: {}", 
                query.getOrganizationId(), query.getSearchTerm());

        // Validate user access to organization
        validationUtils.validateOrganizationAccess(query.getOrganizationId(), query.getUserId());
        
        // Search inventories by term
        List<Inventory> inventories = inventoryRepository.searchInventoriesByOrganization(
                query.getOrganizationId(), 
                query.getSearchTerm()
        );
        
        return inventories;
    }

    @Override
    public Class<SearchInventoriesQuery> getQueryType() {
        return SearchInventoriesQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return false; // Search results shouldn't be cached due to dynamic nature
    }

    @Override
    public String getCacheKey(SearchInventoriesQuery query) {
        return null; // Not cacheable
    }
}