package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.CreateInventoryItemCommand;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryItemRequest;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemCreateValidator;
import com.sajjadkademm.retail.domain.inventory.validation.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.domain.inventory.events.InventoryItemCreatedEvent;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.services.InventoryDomainService;

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
    private final GlobalAuditService globalAuditService;
    private final ApplicationEventPublisher applicationEventPublisher;
    private final InventoryDomainService inventoryDomainService;

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
        
        // Log audit trail
        globalAuditService.auditEntityChange(
                organizationId,
                EntityType.INVENTORY_ITEM,
                savedItem.getId(),
                savedItem.getName(),
                AuditAction.CREATE,
                "Created inventory item: " + savedItem.getName(),
                null, // fieldName
                null, // oldValue
                null, // newValue
                context.getUser()
        );

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
}