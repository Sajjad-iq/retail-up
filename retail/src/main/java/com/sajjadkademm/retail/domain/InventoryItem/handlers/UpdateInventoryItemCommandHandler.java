package com.sajjadkademm.retail.domain.InventoryItem.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.InventoryItem.commands.UpdateInventoryItemCommand;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.domain.InventoryItem.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.InventoryItem.validation.InventoryItemUpdateValidator;
import com.sajjadkademm.retail.domain.InventoryItem.validation.InventoryItemValidationUtils;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.user.model.User;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import jakarta.servlet.http.HttpServletRequest;
import com.sajjadkademm.retail.domain.inventory.services.InventoryDomainService;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for updating inventory items.
 * Pure CQRS implementation using repositories and domain services directly.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateInventoryItemCommandHandler implements CommandHandler<UpdateInventoryItemCommand, InventoryItem> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemUpdateValidator inventoryItemUpdateValidator;
    private final InventoryItemValidationUtils validationUtils;
    private final GlobalAuditRepository auditRepository;
    private final InventoryDomainService inventoryDomainService;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public InventoryItem handle(UpdateInventoryItemCommand command) throws Exception {
        log.debug("Handling UpdateInventoryItemCommand for item: {}", command.getItemId());
        
        // Find existing inventory item
        InventoryItem existingItem = inventoryItemRepository.findById(command.getItemId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage(), command.getItemId())));

        // Validate user access
        validationUtils.checkUserAccessToInventoryItem(existingItem);
        
        // Validate update request
        inventoryItemUpdateValidator.validate(existingItem, command.getRequest());
        
        // Apply updates to existing item
        UpdateInventoryItemRequest request = command.getRequest();
        applyUpdates(existingItem, request);
        
        // Save updated item
        InventoryItem savedItem = inventoryItemRepository.save(existingItem);
        
        // Get organization ID for audit
        String organizationId = getOrganizationId(savedItem);
        
        // Invalidate inventory item-related caches
        cacheInvalidationService.invalidateInventoryItemCaches(
                savedItem.getId(),
                savedItem.getInventoryId(),
                organizationId
        );
        
        // Log audit trail
        auditEntityChange(organizationId, EntityType.INVENTORY_ITEM, savedItem.getId(),
                savedItem.getName(), AuditAction.UPDATE, "Updated inventory item: " + savedItem.getName(),
                null, null, null, savedItem.getCreatedBy());
        
        log.info("Successfully updated inventory item: {} for user: {}", 
                savedItem.getId(), command.getUserId());
        
        return savedItem;
    }
    
    /**
     * Apply updates from request to existing item
     */
    private void applyUpdates(InventoryItem existingItem, UpdateInventoryItemRequest request) {
        // Update basic fields if provided
        if (request.getName() != null) {
            existingItem.setName(request.getName());
        }
        if (request.getDescription() != null) {
            existingItem.setDescription(request.getDescription());
        }
        if (request.getProductCode() != null) {
            existingItem.setProductCode(request.getProductCode());
        }
        if (request.getBarcode() != null) {
            existingItem.setBarcode(request.getBarcode());
        }
        if (request.getCategory() != null) {
            existingItem.setCategory(request.getCategory());
        }
        if (request.getBrand() != null) {
            existingItem.setBrand(request.getBrand());
        }
        if (request.getUnit() != null) {
            existingItem.setUnit(request.getUnit());
        }
        if (request.getWeight() != null) {
            existingItem.setWeight(request.getWeight());
        }
        if (request.getDimensions() != null) {
            existingItem.setDimensions(request.getDimensions());
        }
        if (request.getColor() != null) {
            existingItem.setColor(request.getColor());
        }
        if (request.getSize() != null) {
            existingItem.setSize(request.getSize());
        }
        if (request.getCurrentStock() != null) {
            existingItem.setCurrentStock(request.getCurrentStock());
        }
        if (request.getMinimumStock() != null) {
            existingItem.setMinimumStock(request.getMinimumStock());
        }
        if (request.getMaximumStock() != null) {
            existingItem.setMaximumStock(request.getMaximumStock());
        }
        if (request.getCostPrice() != null) {
            existingItem.setCostPrice(request.getCostPrice());
        }
        if (request.getSellingPrice() != null) {
            existingItem.setSellingPrice(request.getSellingPrice());
        }
        if (request.getDiscountPrice() != null) {
            existingItem.setDiscountPrice(request.getDiscountPrice());
        }
        if (request.getDiscountStartDate() != null) {
            existingItem.setDiscountStartDate(request.getDiscountStartDate());
        }
        if (request.getDiscountEndDate() != null) {
            existingItem.setDiscountEndDate(request.getDiscountEndDate());
        }
        if (request.getSupplierName() != null) {
            existingItem.setSupplierName(request.getSupplierName());
        }
        if (request.getIsPerishable() != null) {
            existingItem.setIsPerishable(request.getIsPerishable());
        }
        if (request.getExpiryDate() != null) {
            existingItem.setExpiryDate(request.getExpiryDate());
        }
        if (request.getIsActive() != null) {
            existingItem.setIsActive(request.getIsActive());
        }
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
    public Class<UpdateInventoryItemCommand> getCommandType() {
        return UpdateInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Update operations need transactions
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