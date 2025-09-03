package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemResult;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemCreateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemUpdateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemValidationUtils;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemValidationUtils.ValidationResult;
import com.sajjadkademm.retail.inventory.InventoryItem.events.InventoryItemCreatedEvent;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.audit.GlobalAuditService;
import com.sajjadkademm.retail.audit.enums.AuditAction;
import com.sajjadkademm.retail.audit.enums.EntityType;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.shared.common.validators.UserValidator;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;

import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Application service that handles CRUD operations for `InventoryItem` and
 * orchestrates related behaviors such as initial stock movements and stock
 * adjustments during updates. Validation responsibilities are delegated to
 * dedicated utility components to keep the service focused on orchestration.
 */
@Service
@RequiredArgsConstructor
public class InventoryItemService {
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemCreateValidator inventoryItemCreateValidator;
    private final InventoryItemUpdateValidator inventoryItemUpdateValidator;
    private final GlobalAuditService globalAuditService; // REPLACED: InventoryMovementService with GlobalAuditService
    private final LocalizedErrorService localizedErrorService;
    private final InventoryItemValidationUtils validationUtils;
    private final UserValidator userValidator;
    private final ApplicationEventPublisher applicationEventPublisher;
    private final InventoryService inventoryService;

    /**
     * Get organization ID from inventory item
     */
    private String getOrganizationId(InventoryItem item) {
        try {
            Inventory inventory = inventoryService.getInventoryById(item.getInventoryId());
            return inventory.getOrganizationId();
        } catch (Exception e) {
            return item.getCreatedBy().getId(); // Fallback to user ID
        }
    }

    /**
     * Create a new inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem createInventoryItem(CreateInventoryItemRequest request) {
        // Validate request using the validator utility
        ValidatedCreateInventoryItemContext context = inventoryItemCreateValidator.validate(request);

        // Create and save the inventory item
        InventoryItem saved = createInventoryItemInternal(request, context.getUser());

        // Publish event for audit logging (decoupled from business transaction)
        applicationEventPublisher.publishEvent(new InventoryItemCreatedEvent(this, saved, context.getUser()));

        return saved;
    }

    /**
     * Create inventory item with validation error collection instead of throwing
     * exceptions.
     * This method is designed for batch operations where we want to collect all
     * errors
     * rather than fail fast on the first validation issue.
     * 
     * @param request The inventory item creation request
     * @return CreateInventoryItemResult containing either the created item or error
     *         message
     */
    @Transactional(rollbackFor = { Exception.class })
    public CreateInventoryItemResult createInventoryItemWithErrorCollection(CreateInventoryItemRequest request) {
        // Get current authenticated user using validation utils pattern
        String userId = SecurityUtils.getCurrentUserId();
        User currentUser = userValidator.validateUserActive(userId);

        // Validate request and collect all errors without throwing exceptions
        ValidationResult validationResult = inventoryItemCreateValidator
                .validateAndCollectErrors(request);

        // Return failure result if validation errors exist
        if (validationResult.hasErrors()) {
            return CreateInventoryItemResult.failure(String.join("; ", validationResult.getErrors()));
        }

        // Create inventory item if validation passes
        return createInventoryItemSafely(request, currentUser);
    }

    /**
     * Helper method to create inventory item with exception handling
     * 
     * @param request The validated inventory item creation request
     * @param user    The user performing the operation
     * @return CreateInventoryItemResult with success or failure status
     */
    private CreateInventoryItemResult createInventoryItemSafely(CreateInventoryItemRequest request, User user) {
        try {
            // Create and save the inventory item
            InventoryItem saved = createInventoryItemInternal(request, user);

            // Publish event for audit logging (decoupled from business transaction)
            applicationEventPublisher.publishEvent(new InventoryItemCreatedEvent(this, saved, user));

            return CreateInventoryItemResult.success(saved);
        } catch (Exception e) {
            return CreateInventoryItemResult.failure(localizedErrorService.getLocalizedMessage(
                    InventoryItemErrorCode.ITEM_CREATION_FAILED.getMessage()) + ": " + e.getMessage());
        }
    }

    /**
     * Internal method to create and save inventory item entity.
     * This method builds the InventoryItem entity from the request and persists it.
     * 
     * @param request The inventory item creation request containing all item data
     * @param user    The user creating the item (used for audit trail)
     * @return The persisted InventoryItem entity
     */
    private InventoryItem createInventoryItemInternal(CreateInventoryItemRequest request, User user) {
        // Build inventory item entity from request data
        InventoryItem item = InventoryItem.builder()
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
                .isPerishable(request.getIsPerishable() != null ? request.getIsPerishable() : false)
                .expiryDate(request.getExpiryDate())
                .inventoryId(request.getInventoryId())
                .isActive(true)
                .createdBy(user)
                .build();

        // Persist inventory item to database
        return inventoryItemRepository.save(item);
    }


    /**
     * Update an existing inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem updateInventoryItem(String id, UpdateInventoryItemRequest request) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage())));

        // Use validator utility for validation
        inventoryItemUpdateValidator.validate(item, request);

        // Capture original stock before applying updates
        Integer originalStock = item.getCurrentStock();

        // Apply field updates using validator utility
        inventoryItemUpdateValidator.applyUpdates(item, request);

        // Save and track changes with global audit
        InventoryItem updated = inventoryItemRepository.save(item);

        // GLOBAL AUDIT: Track inventory item update and stock changes
        // REPLACED: Complex trackStockMovements with simple global audit

        // Log general item update
        String organizationId = getOrganizationId(updated);
        globalAuditService.auditEntityChange(
                organizationId,
                EntityType.INVENTORY_ITEM,
                updated.getId(),
                updated.getName(),
                AuditAction.UPDATE,
                "Inventory item updated",
                null, // Could be enhanced to track specific field changes
                null, // Old values could be tracked if needed
                null, // New values could be tracked if needed
                SecurityUtils.getCurrentUser());

        // Log stock changes if any
        Integer newStock = updated.getCurrentStock();
        if (originalStock != null && newStock != null && !originalStock.equals(newStock)) {
            Integer quantityChange = newStock - originalStock;
            globalAuditService.auditInventoryChange(
                    organizationId, // Already calculated above
                    updated.getId(),
                    updated.getName(),
                    quantityChange > 0 ? "STOCK_IN" : "STOCK_OUT",
                    quantityChange,
                    originalStock,
                    newStock,
                    "Stock updated via item edit",
                    "UPDATE",
                    updated.getId(),
                    SecurityUtils.getCurrentUser());
        }

        return updated;
    }

    /**
     * Get inventory item by ID (only for users with access to the organization)
     */
    public InventoryItem getInventoryItemById(String id) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage())));

        // Check if user has access to the inventory item (user must be the creator of
        // the organization)
        validationUtils.checkUserAccessToInventoryItem(item);

        return item;
    }

    /**
     * Get inventory item by barcode within an inventory (only for users with access
     * to the organization)
     */
    public InventoryItem getInventoryItemByBarcode(String barcode, String inventoryId) {
        InventoryItem item = inventoryItemRepository.findByBarcodeAndInventoryId(barcode, inventoryId)
                .orElseThrow(() -> new NotFoundException(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage()) + " with barcode: " + barcode));

        // Check if user has access to the inventory item
        validationUtils.checkUserAccessToInventoryItem(item);

        return item;
    }

    /**
     * Delete inventory item (hard delete from database)
     * Related inventory movements are automatically deleted via JPA cascade
     * Only organization creators can delete inventory items
     */
    @Transactional(rollbackFor = { Exception.class })
    public void deleteInventoryItem(String id) {
        // Verify item exists and check access before deleting
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage())));

        // Check if user has access to the inventory item
        validationUtils.checkUserAccessToInventoryItem(item);

        // GLOBAL AUDIT: Log item deletion before removing from database
        String organizationId = getOrganizationId(item);
        globalAuditService.auditEntityChange(
                organizationId,
                EntityType.INVENTORY_ITEM,
                item.getId(),
                item.getName(),
                AuditAction.DELETE,
                String.format("Inventory item deleted - had %d units in stock",
                        item.getCurrentStock() != null ? item.getCurrentStock() : 0),
                item.getName(), // Old value (what's being deleted)
                null, // No new value for deletion
                "name", // Field name being affected
                SecurityUtils.getCurrentUser());

        // Delete the inventory item - related movements are automatically deleted via
        // cascade
        // NOTE: With global audit, we no longer need separate inventory movement
        // records
        inventoryItemRepository.deleteById(id);
    }

    /**
     * Get item count in an inventory (only for users with access to the
     * organization)
     */
    public long getItemCountByInventory(String inventoryId) {
        // Check if user has access to the inventory
        validationUtils.checkUserAccessToInventory(inventoryId);

        return inventoryItemRepository.countByInventoryId(inventoryId);
    }

    /**
     * Get active item count in an inventory (only for users with access to the
     * organization)
     */
    public long getActiveItemCountByInventory(String inventoryId) {
        // Check if user has access to the inventory
        validationUtils.checkUserAccessToInventory(inventoryId);

        return inventoryItemRepository.countByInventoryIdAndIsActiveTrue(inventoryId);
    }

    // Paginated methods

    /**
     * Advanced filtering with pagination (only for users with access to the
     * organization)
     */
    public PagedResponse<InventoryItem> filterItemsPaginated(String inventoryId, FilterRequest filterRequest, int page,
            int size, String sortBy, String sortDirection) {
        // Check if user has access to the inventory
        validationUtils.checkUserAccessToInventory(inventoryId);

        Pageable pageable = createPageable(page, size, sortBy, sortDirection);

        Page<InventoryItem> pageResult = inventoryItemRepository.findWithFilters(
                inventoryId,
                filterRequest.getCategory(),
                filterRequest.getBrand(),
                filterRequest.getSupplierName(),
                filterRequest.getColor(),
                filterRequest.getSize(),
                filterRequest.getIsActive(),
                filterRequest.getIsPerishable(),
                filterRequest.getMinStock(),
                filterRequest.getMaxStock(),
                filterRequest.getMinCostPrice(),
                filterRequest.getMaxCostPrice(),
                filterRequest.getMinSellingPrice(),
                filterRequest.getMaxSellingPrice(),
                filterRequest.getSearchTerm(),
                pageable);

        return createPagedResponse(pageResult);
    }

    // Helper methods

    /**
     * Create Pageable object with sorting
     */
    private Pageable createPageable(int page, int size, String sortBy, String sortDirection) {
        // Default values
        if (page < 0)
            page = 0;
        if (size <= 0)
            size = 20;
        if (size > 100)
            size = 100; // Max page size limit
        if (sortBy == null || sortBy.trim().isEmpty())
            sortBy = "createdAt";
        if (sortDirection == null || sortDirection.trim().isEmpty())
            sortDirection = "desc";

        Sort.Direction direction = sortDirection.equalsIgnoreCase("asc") ? Sort.Direction.ASC : Sort.Direction.DESC;
        Sort sort = Sort.by(direction, sortBy);

        return PageRequest.of(page, size, sort);
    }

    /**
     * Convert Spring Data Page to custom PagedResponse
     */
    private <T> PagedResponse<T> createPagedResponse(Page<T> page) {
        return PagedResponse.<T>builder()
                .content(page.getContent())
                .page(page.getNumber())
                .size(page.getSize())
                .totalElements(page.getTotalElements())
                .totalPages(page.getTotalPages())
                .first(page.isFirst())
                .last(page.isLast())
                .hasNext(page.hasNext())
                .hasPrevious(page.hasPrevious())
                .numberOfElements(page.getNumberOfElements())
                .empty(page.isEmpty())
                .build();
    }

    // Security helper methods have been moved to InventoryItemValidationUtils
}