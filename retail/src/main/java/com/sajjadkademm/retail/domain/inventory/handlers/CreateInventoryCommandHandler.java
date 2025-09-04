package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.CreateInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.services.users.UserService;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for creating inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CreateInventoryCommandHandler implements CommandHandler<CreateInventoryCommand, Inventory> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditService globalAuditService;
    private final UserService userService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Inventory handle(CreateInventoryCommand command) throws Exception {
        log.debug("Handling CreateInventoryCommand for user: {}", command.getUserId());

        CreateInventoryRequest request = command.getRequest();
        
        // Validate request and access permissions
        validationUtils.validateCreateRequest(request, command.getUserId());
        
        // Get the current user for createdBy field
        User currentUser = userService.getUserById(command.getUserId());
        
        // Build inventory from request
        Inventory inventory = Inventory.builder()
                .name(request.getName())
                .description(request.getDescription())
                .location(request.getLocation())
                .organizationId(request.getOrganizationId())
                .isActive(true)
                .createdBy(currentUser)
                .build();
        
        // Save inventory
        Inventory savedInventory = inventoryRepository.save(inventory);
        
        // Invalidate inventory-related caches
        cacheInvalidationService.invalidateInventoryCaches(
                savedInventory.getId(), 
                request.getOrganizationId()
        );
        
        // Log audit trail
        globalAuditService.auditEntityChange(
                request.getOrganizationId(),
                EntityType.INVENTORY,
                savedInventory.getId(),
                savedInventory.getName(),
                AuditAction.CREATE,
                "Created inventory: " + savedInventory.getName(),
                null, // fieldName
                null, // oldValue
                null, // newValue
                savedInventory.getCreatedBy()
        );
        
        log.info("Successfully created inventory: {} for user: {}", 
                savedInventory.getId(), command.getUserId());
        
        return savedInventory;
    }

    @Override
    public Class<CreateInventoryCommand> getCommandType() {
        return CreateInventoryCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}