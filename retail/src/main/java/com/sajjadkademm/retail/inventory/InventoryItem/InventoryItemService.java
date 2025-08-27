package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemResult;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemCreateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemUpdateValidator;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.ReferenceType;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.config.locales.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;

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
    private final InventoryItemUpdateValidator inventoryItemUpdateValidator;

    private final InventoryMovementService inventoryMovementService;
    private final InventoryService inventoryService;
    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public InventoryItemService(InventoryItemRepository inventoryItemRepository,
            SystemSettingsService systemSettingsService,
            InventoryItemCreateValidator inventoryItemCreateValidator,
            InventoryItemUpdateValidator inventoryItemUpdateValidator,
            InventoryMovementService inventoryMovementService,
            InventoryService inventoryService,
            LocalizedErrorService localizedErrorService) {
        this.inventoryItemRepository = inventoryItemRepository;
        this.inventoryItemCreateValidator = inventoryItemCreateValidator;
        this.inventoryItemUpdateValidator = inventoryItemUpdateValidator;
        this.inventoryMovementService = inventoryMovementService;
        this.inventoryService = inventoryService;
        this.localizedErrorService = localizedErrorService;
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
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Validate request and collect all errors without throwing exceptions
        InventoryItemCreateValidator.ValidationResult validationResult = inventoryItemCreateValidator
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

            // Record initial stock movement
            recordInitialStockMovement(saved, user);

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
     * Record initial stock movement for newly created item.
     * This creates an inventory movement record to track the initial stock being
     * added.
     * 
     * @param item The newly created inventory item
     * @param user The user who created the item
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
     * Get inventory item by ID (only for users with access to the organization)
     */
    public InventoryItem getInventoryItemById(String id) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage())));

        // Check if user has access to the inventory item (user must be the creator of
        // the organization)
        checkUserAccessToInventoryItem(item);

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
        checkUserAccessToInventoryItem(item);

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
        checkUserAccessToInventoryItem(item);

        // Delete the inventory item - related movements are automatically deleted via
        // cascade
        inventoryItemRepository.deleteById(id);
    }

    /**
     * Get item count in an inventory (only for users with access to the
     * organization)
     */
    public long getItemCountByInventory(String inventoryId) {
        // Check if user has access to the inventory
        checkUserAccessToInventory(inventoryId);

        return inventoryItemRepository.countByInventoryId(inventoryId);
    }

    /**
     * Get active item count in an inventory (only for users with access to the
     * organization)
     */
    public long getActiveItemCountByInventory(String inventoryId) {
        // Check if user has access to the inventory
        checkUserAccessToInventory(inventoryId);

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
        checkUserAccessToInventory(inventoryId);

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

    // Security helper methods

    /**
     * Check if current user has access to the inventory item
     */
    private void checkUserAccessToInventoryItem(InventoryItem item) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Get inventory and check if user has access to it
        Inventory inventory = inventoryService.getInventoryById(item.getInventoryId());
        if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                    UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Check if current user has access to the inventory
     */
    private void checkUserAccessToInventory(String inventoryId) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Get inventory and check access
        Inventory inventory = inventoryService.getInventoryById(inventoryId);
        if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                    UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }
}