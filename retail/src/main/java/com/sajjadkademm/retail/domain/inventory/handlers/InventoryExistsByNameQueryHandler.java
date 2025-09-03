package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.InventoryExistsByNameQuery;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for checking if inventory exists by name
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryExistsByNameQueryHandler implements QueryHandler<InventoryExistsByNameQuery, Boolean> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;

    @Override
    public Boolean handle(InventoryExistsByNameQuery query) throws Exception {
        log.debug("Handling InventoryExistsByNameQuery for name: {} in organization: {}", 
                query.getName(), query.getOrganizationId());

        // Validate user access to organization
        validationUtils.validateOrganizationAccess(query.getOrganizationId(), query.getUserId());
        
        // Check if inventory exists by name in organization
        boolean exists = inventoryRepository.existsByNameAndOrganizationId(
                query.getName(), 
                query.getOrganizationId()
        );
        
        return exists;
    }

    @Override
    public Class<InventoryExistsByNameQuery> getQueryType() {
        return InventoryExistsByNameQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(InventoryExistsByNameQuery query) {
        return "inventory-exists-by-name:" + query.getName() + ":" + 
               query.getOrganizationId() + ":" + query.getUserId();
    }
}