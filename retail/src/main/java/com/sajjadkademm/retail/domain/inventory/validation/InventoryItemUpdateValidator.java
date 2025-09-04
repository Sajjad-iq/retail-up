package com.sajjadkademm.retail.domain.inventory.validation;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryItemRequest;
// REMOVED: Old InventoryMovement imports - now using GlobalAuditService
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemValidationUtils.ValidationResult;

import java.time.LocalDate;
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

}
