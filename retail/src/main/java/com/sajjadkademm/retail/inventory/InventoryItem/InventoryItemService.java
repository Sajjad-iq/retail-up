package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemResult;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.InventoryItemCreateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.InventoryItemUpdateUtils;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;

import org.springframework.beans.factory.annotation.Autowired;
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
public class InventoryItemService {
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemCreateValidator inventoryItemCreateValidator;
    private final InventoryItemUpdateUtils inventoryItemUpdateValidator;

    private final InventoryMovementService inventoryMovementService;

    @Autowired
    public InventoryItemService(InventoryItemRepository inventoryItemRepository,
            SystemSettingsService systemSettingsService,
            InventoryItemCreateValidator inventoryItemCreateValidator,
            InventoryItemUpdateUtils inventoryItemUpdateValidator,
            InventoryMovementService inventoryMovementService) {
        this.inventoryItemRepository = inventoryItemRepository;
        this.inventoryItemCreateValidator = inventoryItemCreateValidator;
        this.inventoryItemUpdateValidator = inventoryItemUpdateValidator;
        this.inventoryMovementService = inventoryMovementService;
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

        // Record initial stock movement
        recordInitialStockMovement(saved, context.getUser());

        return saved;
    }

    /**
     * Create inventory item for Excel upload with result object
     * Returns a result object instead of throwing exceptions
     */
    @Transactional(rollbackFor = { Exception.class })
    public CreateInventoryItemResult createInventoryItemForExcelUpload(CreateInventoryItemRequest request, User user) {
        // Use the new validation method that collects errors
        InventoryItemCreateValidator.ValidationResult validationResult = inventoryItemCreateValidator
                .validateAndCollectErrors(request);

        // Return failure result if validation errors exist
        return validationResult.hasErrors()
                ? CreateInventoryItemResult.failure(String.join("; ", validationResult.getErrors()))
                : createInventoryItemSuccessfully(request, user);
    }

    /**
     * Helper method to create inventory item successfully
     */
    private CreateInventoryItemResult createInventoryItemSuccessfully(CreateInventoryItemRequest request, User user) {
        try {
            InventoryItem saved = createInventoryItemInternal(request, user);
            recordInitialStockMovement(saved, user);
            return CreateInventoryItemResult.success(saved);
        } catch (Exception e) {
            return CreateInventoryItemResult.failure("Failed to create inventory item: " + e.getMessage());
        }
    }

    /**
     * Internal method to create inventory item (extracted logic)
     */
    private InventoryItem createInventoryItemInternal(CreateInventoryItemRequest request, User user) {
        // Create inventory item
        InventoryItem item = InventoryItem.builder()
                .name(request.getName())
                .description(request.getDescription())
                .sku(request.getSku())
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

        // Save inventory item
        return inventoryItemRepository.save(item);
    }

    /**
     * Record initial stock movement for newly created item
     */
    private void recordInitialStockMovement(InventoryItem item, User user) {
        Integer initialStock = item.getCurrentStock();
        // Only record movement if stock is positive
        if (initialStock != null && initialStock > 0) {
            inventoryMovementService.recordStockIn(
                    user,
                    item,
                    initialStock,
                    "Initial stock on item creation",
                    ReferenceType.CREATION,
                    item.getId());
        }
    }

    /**
     * Update an existing inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem updateInventoryItem(String id, UpdateInventoryItemRequest request) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found"));

        // Use validator utility for validation
        inventoryItemUpdateValidator.validate(item, request);

        // Capture original stock before applying updates
        Integer originalStock = item.getCurrentStock();

        // Apply field updates using validator utility
        inventoryItemUpdateValidator.applyUpdates(item, request);

        // Save and track stock movements
        InventoryItem updated = inventoryItemRepository.save(item);
        inventoryItemUpdateValidator.trackStockMovements(updated, request, inventoryMovementService, originalStock);

        return updated;
    }

    /**
     * Get inventory item by ID
     */
    public InventoryItem getInventoryItemById(String id) {
        return inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found"));
    }

    /**
     * Get inventory item by SKU within an inventory
     */
    public InventoryItem getInventoryItemBySku(String sku, String inventoryId) {
        return inventoryItemRepository.findBySkuAndInventoryId(sku, inventoryId)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with SKU: " + sku));
    }

    /**
     * Get inventory item by barcode within an inventory
     */
    public InventoryItem getInventoryItemByBarcode(String barcode, String inventoryId) {
        return inventoryItemRepository.findByBarcodeAndInventoryId(barcode, inventoryId)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with barcode: " + barcode));
    }

    /**
     * Delete inventory item (hard delete from database)
     * Related inventory movements are automatically deleted via JPA cascade
     */
    @Transactional(rollbackFor = { Exception.class })
    public void deleteInventoryItem(String id) {
        // Verify item exists before deleting
        inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found"));

        // Delete the inventory item - related movements are automatically deleted via
        // cascade
        inventoryItemRepository.deleteById(id);
    }

    /**
     * Get item count in an inventory
     */
    public long getItemCountByInventory(String inventoryId) {
        return inventoryItemRepository.countByInventoryId(inventoryId);
    }

    /**
     * Get active item count in an inventory
     */
    public long getActiveItemCountByInventory(String inventoryId) {
        return inventoryItemRepository.countByInventoryIdAndIsActiveTrue(inventoryId);
    }

    // Paginated methods

    /**
     * Advanced filtering with pagination
     */
    public PagedResponse<InventoryItem> filterItemsPaginated(String inventoryId, FilterRequest filterRequest, int page,
            int size, String sortBy, String sortDirection) {
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

}