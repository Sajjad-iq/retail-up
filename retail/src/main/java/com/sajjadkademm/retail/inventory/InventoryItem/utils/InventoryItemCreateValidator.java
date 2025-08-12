package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;

@Component
@RequiredArgsConstructor
public class InventoryItemCreateValidator {

    private final InventoryService inventoryService;
    private final UserService userService;
    private final InventoryItemRepository inventoryItemRepository;

    public ValidatedCreateInventoryItemContext validate(CreateInventoryItemRequest request) {
        // Validate inventory exists
        Inventory inventory = inventoryService.getInventoryById(request.getInventoryId());
        if (inventory == null) {
            throw new NotFoundException("Inventory not found with ID: " + request.getInventoryId());
        }

        // Validate user exists
        User user = userService.getUserById(request.getUserId());
        if (user == null) {
            throw new NotFoundException("User not found with ID: " + request.getUserId());
        }

        // Validate SKU uniqueness within inventory
        if (inventoryItemRepository.existsBySkuAndInventoryId(request.getSku(), request.getInventoryId())) {
            throw new ConflictException("Item with SKU '" + request.getSku() + "' already exists in this inventory");
        }

        // Validate Barcode uniqueness within inventory (if provided)
        if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
            if (inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(), request.getInventoryId())) {
                throw new ConflictException(
                        "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
            }
        }

        return new ValidatedCreateInventoryItemContext(inventory, user);
    }
}
