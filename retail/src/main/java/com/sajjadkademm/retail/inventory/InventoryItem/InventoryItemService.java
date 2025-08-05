package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class InventoryItemService {
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryService inventoryService;
    private final UserService userService;

    @Autowired
    public InventoryItemService(InventoryItemRepository inventoryItemRepository,
            InventoryService inventoryService,
            UserService userService) {
        this.inventoryItemRepository = inventoryItemRepository;
        this.inventoryService = inventoryService;
        this.userService = userService;
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

            InventoryItem item = InventoryItem.builder()
                    .name(request.getName())
                    .description(request.getDescription())
                    .sku(request.getSku())
                    .barcode(request.getBarcode())
                    .category(request.getCategory())
                    .brand(request.getBrand())
                    .unit(request.getUnit())
                    .currentStock(request.getCurrentStock())
                    .minimumStock(request.getMinimumStock())
                    .maximumStock(request.getMaximumStock())
                    .costPrice(request.getCostPrice())
                    .sellingPrice(request.getSellingPrice())
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
            item.setCostPrice(request.getCostPrice());
        }
        if (request.getSellingPrice() != null) {
            item.setSellingPrice(request.getSellingPrice());
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
     * Get all items in an inventory
     */
    public List<InventoryItem> getItemsByInventory(String inventoryId) {
        return inventoryItemRepository.findByInventoryId(inventoryId);
    }

    /**
     * Get active items in an inventory
     */
    public List<InventoryItem> getActiveItemsByInventory(String inventoryId) {
        return inventoryItemRepository.findByInventoryIdAndIsActiveTrue(inventoryId);
    }

    /**
     * Get items by category within an inventory
     */
    public List<InventoryItem> getItemsByCategory(String inventoryId, String category) {
        return inventoryItemRepository.findByInventoryIdAndCategory(inventoryId, category);
    }

    /**
     * Get low stock items in an inventory
     */
    public List<InventoryItem> getLowStockItems(String inventoryId) {
        return inventoryItemRepository.findLowStockItems(inventoryId);
    }

    /**
     * Get out of stock items in an inventory
     */
    public List<InventoryItem> getOutOfStockItems(String inventoryId) {
        return inventoryItemRepository.findOutOfStockItems(inventoryId);
    }

    /**
     * Search items by name, SKU, or barcode within an inventory
     */
    public List<InventoryItem> searchItems(String inventoryId, String searchTerm) {
        return inventoryItemRepository.searchItems(inventoryId, searchTerm);
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
     * Update stock quantity for an item
     */
    public InventoryItem updateStock(String id, Integer newStock) {
        if (newStock < 0) {
            throw new BadRequestException("Stock cannot be negative");
        }

        InventoryItem item = inventoryItemRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory item not found with ID: " + id));

        item.setCurrentStock(newStock);
        return inventoryItemRepository.save(item);
    }

    /**
     * Check if SKU exists within an inventory
     */
    public boolean itemExistsBySku(String sku, String inventoryId) {
        return inventoryItemRepository.existsBySkuAndInventoryId(sku, inventoryId);
    }

    /**
     * Check if barcode exists within an inventory
     */
    public boolean itemExistsByBarcode(String barcode, String inventoryId) {
        return inventoryItemRepository.existsByBarcodeAndInventoryId(barcode, inventoryId);
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
}