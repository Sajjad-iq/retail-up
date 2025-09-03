package com.sajjadkademm.retail.inventory.InventoryItem.validator;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryItemRequest;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemValidationUtils.ValidationResult;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class InventoryItemCreateValidator {

    private final InventoryItemValidationUtils validationUtils;

    /**
     * Validate request and collect all errors instead of throwing exceptions
     */
    public ValidationResult validateAndCollectErrors(CreateInventoryItemRequest request) {
        List<String> errors = new ArrayList<>();

        // Resolve and validate inventory
        Inventory inventory = validationUtils.validateAndResolveInventory(request.getInventoryId(), errors);

        // Validate basic required fields
        validationUtils.validateBasicFields(request.getName(), request.getUnit(),
                request.getCurrentStock(), request.getMinimumStock(), errors);

        // Validate organization
        validationUtils.validateOrganization(inventory, errors);

        // Validate user and ensure it is active
        User user = validationUtils.validateUser(errors);

        // Check if user has access to the organization (user must be the creator of the
        // organization)
        validationUtils.validateOrganizationCreatorAccess(user, inventory, errors);

        // Normalize string inputs
        String[] barcode = { request.getBarcode() };
        String[] productCode = { request.getProductCode() };
        validationUtils.normalizeStringInputs(barcode, productCode);
        request.setBarcode(barcode[0]);
        request.setProductCode(productCode[0]);

        // Validate uniqueness within inventory scope
        if (inventory != null) {
            validationUtils.validateBarcodeUniqueness(request.getBarcode(),
                    request.getInventoryId(), null, false, errors);
            validationUtils.validateProductCodeUniqueness(request.getProductCode(),
                    request.getInventoryId(), null, false, errors);
        }

        // Validate stock cross-field rules
        validationUtils.validateStockRules(request.getMinimumStock(),
                request.getMaximumStock(), request.getCurrentStock(), errors);

        // Validate pricing
        Money costPrice = request.getCostPrice();
        Money sellingPrice = request.getSellingPrice();

        validationUtils.validateCurrencyRequirements(costPrice, sellingPrice, false, errors);
        validationUtils.validatePriceRelationships(costPrice, sellingPrice, errors);

        // Validate discount rules
        validationUtils.validateDiscountRules(request.getDiscountPrice(),
                request.getDiscountStartDate(), request.getDiscountEndDate(), sellingPrice, false, errors);

        // Validate perishable item rules
        validationUtils.validatePerishableRules(request.getIsPerishable(), request.getExpiryDate(), errors);

        return new ValidationResult(errors, inventory, user);
    }

    /**
     * Validate request and return context or throw exception
     * Consolidates validation to use single approach
     */
    public ValidatedCreateInventoryItemContext validate(CreateInventoryItemRequest request) {
        ValidationResult result = validateAndCollectErrors(request);

        if (result.hasErrors()) {
            // Throw exception with all errors combined for better user experience
            String combinedErrors = String.join("; ", result.getErrors());
            throw new BadRequestException(combinedErrors);
        }

        return new ValidatedCreateInventoryItemContext(result.getInventory(), result.getUser());
    }
}
