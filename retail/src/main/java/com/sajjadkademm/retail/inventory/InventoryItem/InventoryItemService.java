package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.InventoryItemCreateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.ValidatedCreateInventoryItemContext;
import com.sajjadkademm.retail.inventory.InventoryItem.utils.InventoryItemUpdateUtils;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
// removed unused UserService import
import com.sajjadkademm.retail.utils.dto.Currency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
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
    private final SystemSettingsService systemSettingsService;
    private final InventoryItemCreateValidator inventoryItemCreateValidator;
    private final InventoryItemUpdateUtils inventoryItemUpdateValidator;

    // Optional lazy injection to avoid circular dependency with
    // InventoryMovementService
    @Autowired(required = false)
    @Lazy
    private InventoryMovementService inventoryMovementService;

    @Autowired
    public InventoryItemService(InventoryItemRepository inventoryItemRepository,
            SystemSettingsService systemSettingsService,
            InventoryItemCreateValidator inventoryItemCreateValidator,
            InventoryItemUpdateUtils inventoryItemUpdateValidator) {
        this.inventoryItemRepository = inventoryItemRepository;
        this.systemSettingsService = systemSettingsService;
        this.inventoryItemCreateValidator = inventoryItemCreateValidator;
        this.inventoryItemUpdateValidator = inventoryItemUpdateValidator;
    }

    /**
     * Create a new inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem createInventoryItem(CreateInventoryItemRequest request) {
        try {
            // Validate request and fetch required entities
            ValidatedCreateInventoryItemContext context = inventoryItemCreateValidator.validate(request);
            Inventory inventory = context.getInventory();
            User user = context.getUser();

            // get currency from organization settings
            Currency currency = resolveCurrency(inventory.getOrganizationId());

            // get initial stock from request
            Integer initialStock = request.getCurrentStock();

            // create inventory item
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
                    .costPrice(request.getCostPrice() != null ? new Money(request.getCostPrice(), currency) : null)
                    .sellingPrice(new Money(request.getSellingPrice(), currency))
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

            // save inventory item
            InventoryItem saved = inventoryItemRepository.save(item);

            // record initial STOCK_IN movement via movement service helpers
            if (inventoryMovementService != null && initialStock != null && initialStock > 0) {
                inventoryMovementService.recordStockIn(
                        user,
                        saved,
                        initialStock,
                        "Initial stock on item creation",
                        com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType.ADJUSTMENT,
                        saved.getId());
            }

            return saved;

        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to create inventory item: " + e.getMessage(), e);
        }
    }

    /**
     * Update an existing inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem updateInventoryItem(String id, UpdateInventoryItemRequest request) {
        try {
            InventoryItem item = inventoryItemRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + id));

            // validation
            inventoryItemUpdateValidator.validate(item, request);

            // capture current stock before applying updates
            Integer requestedStock = request.getCurrentStock();

            // apply field updates
            inventoryItemUpdateValidator.applyUpdates(item, request);

            // record stock movement if stock changed (use saved item to keep update order
            // logical)
            InventoryItem updated = inventoryItemRepository.save(item);
            if (requestedStock != null && inventoryMovementService != null) {
                // attribute to item's creator by default
                User actor = updated.getCreatedBy();
                inventoryMovementService.recordAdjustmentToTarget(
                        actor,
                        updated,
                        requestedStock,
                        "Stock adjusted via item update",
                        com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType.ADJUSTMENT,
                        updated.getId());
            }
            return updated;
        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to update inventory item: " + e.getMessage(), e);
        }
    }

    /**
     * Get inventory item by ID
     */
    public InventoryItem getInventoryItemById(String id) {
        return inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + id));
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
     * Delete inventory item (soft delete by setting isActive to false)
     */
    public void deleteInventoryItem(String id) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + id));

        item.setIsActive(false);
        inventoryItemRepository.save(item);
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

    /**
     * Update stock for an inventory item (internal use)
     */
    public InventoryItem updateStock(String itemId, Integer newStock) {
        InventoryItem item = inventoryItemRepository.findById(itemId)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + itemId));

        item.setCurrentStock(newStock);
        return inventoryItemRepository.save(item);
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

    private Currency resolveCurrency(String organizationId) {
        try {
            SystemSetting systemSetting = systemSettingsService.getSystemSettings(organizationId);
            String currencyCode = systemSetting.getCurrency();
            if (currencyCode == null || currencyCode.isBlank()) {
                return Currency.USD;
            }
            try {
                return Currency.valueOf(currencyCode.toUpperCase());
            } catch (IllegalArgumentException ex) {
                return Currency.USD;
            }
        } catch (Exception ex) {
            return Currency.USD;
        }
    }

}