package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.utils.dto.Currency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

@Service
public class InventoryItemService {
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryService inventoryService;
    private final SystemSettingsService systemSettingsService;
    private final UserService userService;

    @Autowired
    public InventoryItemService(InventoryItemRepository inventoryItemRepository,
            InventoryService inventoryService,
            UserService userService,
            SystemSettingsService systemSettingsService) {
        this.inventoryItemRepository = inventoryItemRepository;
        this.inventoryService = inventoryService;
        this.userService = userService;
        this.systemSettingsService = systemSettingsService;
    }

    /**
     * Create a new inventory item
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryItem createInventoryItem(CreateInventoryItemRequest request) {
        try {
            // Check if inventory exists
            Inventory inventory = inventoryService.getInventoryById(request.getInventoryId());
            if (inventory == null) {
                throw new NotFoundException("Inventory not found with ID: " + request.getInventoryId());
            }

            // Check if user exists
            User user = userService.getUserById(request.getUserId());
            if (user == null) {
                throw new NotFoundException("User not found with ID: " + request.getUserId());
            }

            // Check if SKU already exists in the inventory
            if (inventoryItemRepository.existsBySkuAndInventoryId(request.getSku(), request.getInventoryId())) {
                throw new ConflictException(
                        "Item with SKU '" + request.getSku() + "' already exists in this inventory");
            }

            // Check if barcode already exists in the inventory (if provided)
            if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
                if (inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(),
                        request.getInventoryId())) {
                    throw new ConflictException(
                            "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
                }
            }

            Currency currency = resolveCurrency(inventory.getOrganizationId());

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

            return inventoryItemRepository.save(item);

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
    public InventoryItem updateInventoryItem(String id, UpdateInventoryItemRequest request) {
        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + id));

        // Check if new barcode conflicts with existing item in the same inventory
        if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
            if (!request.getBarcode().equals(item.getBarcode()) &&
                    inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(),
                            item.getInventoryId())) {
                throw new ConflictException(
                        "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
            }
        }

        // Update fields if provided
        if (request.getName() != null) {
            item.setName(request.getName());
        }
        if (request.getDescription() != null) {
            item.setDescription(request.getDescription());
        }
        if (request.getProductCode() != null) {
            item.setProductCode(request.getProductCode());
        }
        if (request.getBarcode() != null) {
            item.setBarcode(request.getBarcode());
        }
        if (request.getCategory() != null) {
            item.setCategory(request.getCategory());
        }
        if (request.getBrand() != null) {
            item.setBrand(request.getBrand());
        }
        if (request.getUnit() != null) {
            item.setUnit(request.getUnit());
        }
        if (request.getWeight() != null) {
            item.setWeight(request.getWeight());
        }
        if (request.getDimensions() != null) {
            item.setDimensions(request.getDimensions());
        }
        if (request.getColor() != null) {
            item.setColor(request.getColor());
        }
        if (request.getSize() != null) {
            item.setSize(request.getSize());
        }
        if (request.getCurrentStock() != null) {
            item.setCurrentStock(request.getCurrentStock());
        }
        if (request.getMinimumStock() != null) {
            item.setMinimumStock(request.getMinimumStock());
        }
        if (request.getMaximumStock() != null) {
            item.setMaximumStock(request.getMaximumStock());
        }
        if (request.getCostPrice() != null) {
            Currency currency = resolveCurrency(item.getInventory().getOrganizationId());
            if (item.getCostPrice() == null) {
                item.setCostPrice(new Money(request.getCostPrice(), currency));
            } else {
                item.getCostPrice().setAmount(request.getCostPrice());
                item.getCostPrice().setCurrency(currency);
            }
        }
        if (request.getSellingPrice() != null) {
            Currency currency = resolveCurrency(item.getInventory().getOrganizationId());
            if (item.getSellingPrice() == null) {
                item.setSellingPrice(new Money(request.getSellingPrice(), currency));
            } else {
                item.getSellingPrice().setAmount(request.getSellingPrice());
                item.getSellingPrice().setCurrency(currency);
            }
        }
        if (request.getDiscountPrice() != null) {
            item.setDiscountPrice(request.getDiscountPrice());
        }
        if (request.getDiscountStartDate() != null) {
            item.setDiscountStartDate(request.getDiscountStartDate());
        }
        if (request.getDiscountEndDate() != null) {
            item.setDiscountEndDate(request.getDiscountEndDate());
        }
        if (request.getSupplierName() != null) {
            item.setSupplierName(request.getSupplierName());
        }
        if (request.getIsPerishable() != null) {
            item.setIsPerishable(request.getIsPerishable());
        }
        if (request.getExpiryDate() != null) {
            item.setExpiryDate(request.getExpiryDate());
        }
        if (request.getIsActive() != null) {
            item.setIsActive(request.getIsActive());
        }

        return inventoryItemRepository.save(item);
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