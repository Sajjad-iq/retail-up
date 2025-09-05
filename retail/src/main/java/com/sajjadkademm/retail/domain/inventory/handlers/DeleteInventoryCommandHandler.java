package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.DeleteInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import org.springframework.stereotype.Component;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for deleting inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DeleteInventoryCommandHandler implements CommandHandler<DeleteInventoryCommand, Void> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditRepository auditRepository;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Void handle(DeleteInventoryCommand command) throws Exception {
        log.debug("Handling DeleteInventoryCommand for inventory: {}", command.getInventoryId());

        // Find existing inventory
        Inventory existingInventory = inventoryRepository.findById(command.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), command.getInventoryId())));

        // Validate user access and delete permissions
        validationUtils.validateDeleteAccess(existingInventory, command.getUserId());
        
        String inventoryName = existingInventory.getName();
        String organizationId = existingInventory.getOrganizationId();
        
        // Delete inventory
        inventoryRepository.delete(existingInventory);
        
        // Invalidate caches
        cacheInvalidationService.invalidateInventoryCaches(command.getInventoryId(), organizationId);
        
        // Log audit trail
        auditEntityChange(
                organizationId,
                EntityType.INVENTORY,
                command.getInventoryId(),
                inventoryName,
                AuditAction.DELETE,
                "Deleted inventory: " + inventoryName,
                null,
                null,
                null,
                existingInventory.getCreatedBy()
        );
        
        log.info("Successfully deleted inventory: {} for user: {}", 
                command.getInventoryId(), command.getUserId());
        
        return null;
    }

    @Override
    public Class<DeleteInventoryCommand> getCommandType() {
        return DeleteInventoryCommand.class;
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