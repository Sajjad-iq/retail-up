package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.UpdateInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import jakarta.servlet.http.HttpServletRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for updating inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateInventoryCommandHandler implements CommandHandler<UpdateInventoryCommand, Inventory> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditRepository auditRepository;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Inventory handle(UpdateInventoryCommand command) throws Exception {
        log.debug("Handling UpdateInventoryCommand for inventory: {}", command.getInventoryId());

        // Find existing inventory
        Inventory existingInventory = inventoryRepository.findById(command.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), command.getInventoryId())));

        // Validate user access and update request
        validationUtils.validateUpdateAccess(existingInventory, command.getUserId());
        validationUtils.validateUpdateRequest(command.getRequest(), existingInventory);
        
        // Apply updates
        UpdateInventoryRequest request = command.getRequest();
        applyUpdates(existingInventory, request);
        
        // Save updated inventory
        Inventory savedInventory = inventoryRepository.save(existingInventory);
        
        // Invalidate inventory-related caches
        cacheInvalidationService.invalidateInventoryCaches(
                savedInventory.getId(), 
                savedInventory.getOrganizationId()
        );
        
        // Log audit trail
        auditEntityChange(
                savedInventory.getOrganizationId(),
                EntityType.INVENTORY,
                savedInventory.getId(),
                savedInventory.getName(),
                AuditAction.UPDATE,
                "Updated inventory: " + savedInventory.getName(),
                null, // fieldName
                null, // oldValue
                null, // newValue
                savedInventory.getCreatedBy()
        );
        
        log.info("Successfully updated inventory: {} for user: {}", 
                savedInventory.getId(), command.getUserId());
        
        return savedInventory;
    }
    
    /**
     * Apply updates from request to existing inventory
     */
    private void applyUpdates(Inventory existingInventory, UpdateInventoryRequest request) {
        if (request.getName() != null) {
            existingInventory.setName(request.getName());
        }
        if (request.getDescription() != null) {
            existingInventory.setDescription(request.getDescription());
        }
        if (request.getLocation() != null) {
            existingInventory.setLocation(request.getLocation());
        }
        if (request.getIsActive() != null) {
            existingInventory.setIsActive(request.getIsActive());
        }
    }

    @Override
    public Class<UpdateInventoryCommand> getCommandType() {
        return UpdateInventoryCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }

    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    private void auditEntityChange(String organizationId, EntityType entityType, String entityId,
            String entityName, AuditAction action, String description,
            String fieldName, String oldValue, String newValue, User user) {
        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(entityType)
                    .entityId(entityId)
                    .entityName(entityName)
                    .action(action)
                    .description(description)
                    .fieldName(fieldName)
                    .oldValue(oldValue)
                    .newValue(newValue)
                    .businessProcess("Entity Management")
                    .performedBy(user)
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(action.isHighRisk() || entityType.isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {}", action, entityName, user.getEmail());

        } catch (Exception e) {
            log.error("Failed to log entity audit for {} {}: {}", entityType, entityId, e.getMessage(), e);
        }
    }

}