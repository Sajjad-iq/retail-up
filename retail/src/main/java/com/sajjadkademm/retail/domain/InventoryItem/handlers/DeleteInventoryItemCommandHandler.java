package com.sajjadkademm.retail.domain.InventoryItem.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.InventoryItem.commands.DeleteInventoryItemCommand;
import com.sajjadkademm.retail.domain.InventoryItem.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.InventoryItem.validation.InventoryItemValidationUtils;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.inventory.services.InventoryDomainService;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

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
 * Command handler for deleting inventory items.
 * Pure CQRS implementation using repositories and domain services directly.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DeleteInventoryItemCommandHandler implements CommandHandler<DeleteInventoryItemCommand, Void> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;
    private final GlobalAuditRepository auditRepository;
    private final InventoryDomainService inventoryDomainService;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Void handle(DeleteInventoryItemCommand command) throws Exception {
        log.debug("Handling DeleteInventoryItemCommand for item: {}", command.getItemId());
        
        // Find existing inventory item
        InventoryItem existingItem = inventoryItemRepository.findById(command.getItemId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage(), command.getItemId())));

        // Validate user access
        validationUtils.checkUserAccessToInventoryItem(existingItem);
        
        // Get organization ID for audit (before deletion)
        String organizationId = getOrganizationId(existingItem);
        String itemName = existingItem.getName();
        
        // Delete inventory item
        inventoryItemRepository.delete(existingItem);
        
        // Invalidate caches
        cacheInvalidationService.invalidateInventoryItemCaches(command.getItemId(), existingItem.getInventoryId(), organizationId);
        
        // Log audit trail
        auditEntityChange(
                organizationId,
                EntityType.INVENTORY_ITEM,
                command.getItemId(),
                itemName,
                AuditAction.DELETE,
                "Deleted inventory item: " + itemName,
                null,
                null,
                null,
                existingItem.getCreatedBy()
        );
        
        log.info("Successfully deleted inventory item: {} for user: {}", 
                command.getItemId(), command.getUserId());
        
        return null;
    }
    
    /**
     * Get organization ID from inventory item
     */
    private String getOrganizationId(InventoryItem item) {
        try {
            return inventoryDomainService.getOrganizationIdByInventory(item.getInventoryId());
        } catch (Exception e) {
            return item.getCreatedBy().getId(); // Fallback to user ID
        }
    }

    @Override
    public Class<DeleteInventoryItemCommand> getCommandType() {
        return DeleteInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Delete operations need transactions
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
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(action.isHighRisk() || entityType.isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {}", action, entityName, user.getEmail());

        } catch (Exception e) {
            log.error("Failed to log entity audit for {} {}: {}", entityType, entityId, e.getMessage(), e);
        }
    }

    private String getClientIp() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                HttpServletRequest request = attributes.getRequest();
                String xForwardedFor = request.getHeader("X-Forwarded-For");
                if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
                    return xForwardedFor.split(",")[0].trim();
                }
                return request.getRemoteAddr();
            }
        } catch (Exception e) {
            // Ignore - audit context may not have request
        }
        return "unknown";
    }

    private String getUserAgent() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                return attributes.getRequest().getHeader("User-Agent");
            }
        } catch (Exception e) {
            // Ignore - audit context may not have request
        }
        return "unknown";
    }
}