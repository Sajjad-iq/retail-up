package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.CreateInventoryItemCommand;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryItemRequest;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemCreateValidator;
import com.sajjadkademm.retail.domain.inventory.validation.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.domain.inventory.events.InventoryItemCreatedEvent;
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
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.services.InventoryDomainService;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for creating inventory items.
 * Pure CQRS implementation using repositories and domain services directly.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CreateInventoryItemCommandHandler implements CommandHandler<CreateInventoryItemCommand, InventoryItem> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemCreateValidator inventoryItemCreateValidator;
    private final GlobalAuditRepository auditRepository;
    private final ApplicationEventPublisher applicationEventPublisher;
    private final InventoryDomainService inventoryDomainService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public InventoryItem handle(CreateInventoryItemCommand command) throws Exception {
        log.debug("Handling CreateInventoryItemCommand for user: {}", command.getUserId());
        
        // Validate request using the validator utility
        ValidatedCreateInventoryItemContext context = inventoryItemCreateValidator.validate(command.getRequest());
        
        // Get the request for building the item
        CreateInventoryItemRequest request = command.getRequest();
        
        // Build inventory item from request data
        InventoryItem inventoryItem = InventoryItem.builder()
                .name(request.getName())
                .description(request.getDescription())
                .productCode(request.getProductCode())
                .barcode(request.getBarcode())
                .category(request.getCategory())
                .brand(request.getBrand())
                .unit(request.getUnit())
                .weight(request.getWeight())
                .dimensions(request.getDimensions())
                .color(request.getColor())
                .size(request.getSize())
                .currentStock(request.getCurrentStock())
                .minimumStock(request.getMinimumStock())
                .maximumStock(request.getMaximumStock())
                .costPrice(request.getCostPrice())
                .sellingPrice(request.getSellingPrice())
                .discountPrice(request.getDiscountPrice())
                .discountStartDate(request.getDiscountStartDate())
                .discountEndDate(request.getDiscountEndDate())
                .supplierName(request.getSupplierName())
                .isPerishable(request.getIsPerishable())
                .expiryDate(request.getExpiryDate())
                .isActive(true)
                .inventoryId(request.getInventoryId())
                .createdBy(context.getUser())
                .build();

        // Save inventory item
        InventoryItem savedItem = inventoryItemRepository.save(inventoryItem);
        
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
                savedItem.getName(), AuditAction.CREATE, "Created inventory item: " + savedItem.getName(),
                null, null, null, context.getUser());

        // Publish domain event
        applicationEventPublisher.publishEvent(new InventoryItemCreatedEvent(this, savedItem, context.getUser()));
        
        log.info("Successfully created inventory item: {} for user: {}", 
                savedItem.getId(), command.getUserId());
        
        return savedItem;
    }
    
    /**
     * Get organization ID from inventory item
     */
    private String getOrganizationId(InventoryItem item) {
        try {
            Inventory inventory = inventoryDomainService.getInventoryById(item.getInventoryId());
            return inventory.getOrganizationId();
        } catch (Exception e) {
            return item.getCreatedBy().getId(); // Fallback to user ID
        }
    }

    @Override
    public Class<CreateInventoryItemCommand> getCommandType() {
        return CreateInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Creation operations need transactions
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