package com.sajjadkademm.retail.inventory.InventoryItem.validator;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.ReferenceType;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemValidationUtils.ValidationResult;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class InventoryItemUpdateValidator {

    private final InventoryItemValidationUtils validationUtils;

    /**
     * Validate update request and collect all errors instead of throwing exceptions
     */
    public ValidationResult validateAndCollectErrors(InventoryItem existing, UpdateInventoryItemRequest request) {
        List<String> errors = new ArrayList<>();

        // Resolve and validate inventory
        Inventory inventory = validationUtils.validateAndResolveInventory(existing.getInventoryId(), errors);

        // Validate basic required fields
        validationUtils.validateBasicFields(request.getName(), request.getUnit(),
                request.getCurrentStock(), request.getMinimumStock(), errors);

        // Validate organization
        validationUtils.validateOrganization(inventory, errors);

        // Validate user and ensure it is active (no organization creator check for
        // updates)
        User user = validationUtils.validateUser(errors);

        // Normalize string inputs
        String[] barcode = { request.getBarcode() };
        String[] productCode = { request.getProductCode() };
        validationUtils.normalizeStringInputs(barcode, productCode);
        request.setBarcode(barcode[0]);
        request.setProductCode(productCode[0]);

        // Validate uniqueness within inventory scope (only when changed)
        if (inventory != null) {
            validationUtils.validateBarcodeUniqueness(request.getBarcode(),
                    existing.getInventoryId(), existing.getBarcode(), true, errors);
            validationUtils.validateProductCodeUniqueness(request.getProductCode(),
                    existing.getInventoryId(), existing.getProductCode(), true, errors);
        }

        // Cross-field rules (use new value if present, otherwise existing)
        Integer minStock = request.getMinimumStock() != null ? request.getMinimumStock() : existing.getMinimumStock();
        Integer maxStock = request.getMaximumStock() != null ? request.getMaximumStock() : existing.getMaximumStock();
        Integer currentStock = request.getCurrentStock() != null ? request.getCurrentStock()
                : existing.getCurrentStock();

        validationUtils.validateStockRules(minStock, maxStock, currentStock, errors);

        // Validate pricing
        Money costPrice = request.getCostPrice() != null ? request.getCostPrice() : existing.getCostPrice();
        Money sellingPrice = request.getSellingPrice() != null ? request.getSellingPrice() : existing.getSellingPrice();

        validationUtils.validateCurrencyRequirements(request.getCostPrice(), request.getSellingPrice(), true, errors);
        validationUtils.validatePriceRelationships(costPrice, sellingPrice, errors);

        // Validate discount rules
        validationUtils.validateDiscountRules(request.getDiscountPrice(),
                request.getDiscountStartDate(), request.getDiscountEndDate(), sellingPrice, true, errors);

        // Validate perishable item rules
        Boolean isPerishable = request.getIsPerishable() != null ? request.getIsPerishable()
                : existing.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate() != null ? request.getExpiryDate() : existing.getExpiryDate();
        validationUtils.validatePerishableRules(isPerishable, expiryDate, errors);

        return new ValidationResult(errors, inventory, user);
    }

    /**
     * Original validate method for backward compatibility (throws exceptions)
     */
    public void validate(InventoryItem existing, UpdateInventoryItemRequest request) {
        ValidationResult result = validateAndCollectErrors(existing, request);

        if (result.hasErrors()) {
            // For backward compatibility, throw the first error
            throw new BadRequestException(result.getErrors().get(0));
        }
    }

    public void applyUpdates(InventoryItem item, UpdateInventoryItemRequest request) {
        // Apply each change only when provided
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
            item.setCostPrice(request.getCostPrice());
        }
        if (request.getSellingPrice() != null) {
            item.setSellingPrice(request.getSellingPrice());
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
    }

    /**
     * Track inventory movements and changes for all relevant field updates
     */
    public void trackStockMovements(InventoryItem item, UpdateInventoryItemRequest request,
            InventoryMovementService inventoryMovementService, Integer originalStock) {

        // Resolve user and ensure it is active
        User actor = SecurityUtils.getCurrentUser();

        // Track stock changes
        if (request.getCurrentStock() != null && !request.getCurrentStock().equals(originalStock)) {
            int stockDifference = request.getCurrentStock() - originalStock;
            String movementDescription = stockDifference > 0 ? "Stock increased via item update"
                    : "Stock decreased via item update";

            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    request.getCurrentStock(),
                    movementDescription,
                    ReferenceType.ADJUSTMENT,
                    item.getId());
        }

        // Track pricing changes
        if (request.getCostPrice() != null
                && !request.getCostPrice().getAmount().equals(item.getCostPrice().getAmount())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "Cost price updated via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

        if (request.getSellingPrice() != null
                && !request.getSellingPrice().getAmount().equals(item.getSellingPrice().getAmount())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "Selling price updated via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

        // Track discount changes
        if (request.getDiscountPrice() != null && !request.getDiscountPrice().equals(item.getDiscountPrice())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "Discount price updated via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

        // Track critical business field changes
        if (request.getIsActive() != null && !request.getIsActive().equals(item.getIsActive())) {
            String statusChange = request.getIsActive() ? "Item activated" : "Item deactivated";
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    statusChange + " via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

        // Track supplier changes
        if (request.getSupplierName() != null && !request.getSupplierName().equals(item.getSupplierName())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "Supplier changed via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

        // Track expiry date changes for perishable items
        if (request.getExpiryDate() != null && !request.getExpiryDate().equals(item.getExpiryDate())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "Expiry date updated via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }
    }

}
