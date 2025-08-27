package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import com.sajjadkademm.retail.config.locales.errorCode.InventoryErrorCode;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;

import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.organizations.utils.OrganizationValidationUtils;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.organizations.OrganizationErrorCode;
import com.sajjadkademm.retail.users.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class InventoryItemCreateValidator {

    private final InventoryService inventoryService;
    private final OrganizationService organizationService;
    private final InventoryItemRepository inventoryItemRepository;
    private final LocalizedErrorService localizedErrorService;
    private final OrganizationValidationUtils organizationValidationUtils;

    /**
     * Validation result containing errors and validated entities
     */
    public static class ValidationResult {
        private final List<String> errors;
        private final Inventory inventory;
        private final User user;
        private final boolean isValid;

        public ValidationResult(List<String> errors, Inventory inventory, User user) {
            this.errors = errors;
            this.inventory = inventory;
            this.user = user;
            this.isValid = errors.isEmpty();
        }

        public List<String> getErrors() {
            return errors;
        }

        public Inventory getInventory() {
            return inventory;
        }

        public User getUser() {
            return user;
        }

        public boolean isValid() {
            return isValid;
        }

        public boolean hasErrors() {
            return !errors.isEmpty();
        }
    }

    /**
     * Validate request and collect all errors instead of throwing exceptions
     */
    public ValidationResult validateAndCollectErrors(CreateInventoryItemRequest request) {
        List<String> errors = new ArrayList<>();
        Inventory inventory = null;
        User user = null;

        try {
            // Resolve and validate inventory
            inventory = inventoryService.getInventoryById(request.getInventoryId());
            if (inventory == null) {
                errors.add(
                        localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage()));
            }
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage())
                    + ": " + e.getMessage());
        }

        // name is required
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryErrorCode.NAME_REQUIRED.getMessage()));
        }

        // unit is required
        if (request.getUnit() == null) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryErrorCode.UNIT_REQUIRED.getMessage()));
        }

        // current stock must be greater than or equal to 0
        if (request.getCurrentStock() == null || request.getCurrentStock() < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }

        // minimum stock must be greater than or equal to 0
        if (request.getMinimumStock() != null && request.getMinimumStock() < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }

        // Validate organization if inventory was resolved
        if (inventory != null) {
            try {
                Organization organization = organizationService.getOrganizationById(inventory.getOrganizationId());
                if (organization == null) {
                    errors.add(localizedErrorService
                            .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()));
                } else {
                    try {
                        organizationValidationUtils.assertOrganizationIsActive(organization);
                    } catch (Exception e) {
                        errors.add(localizedErrorService.getLocalizedMessage(
                                OrganizationErrorCode.ORGANIZATION_VALIDATION_FAILED.getMessage()) + ": "
                                + e.getMessage());
                    }
                }
            } catch (Exception e) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()) + ": " + e.getMessage());
            }

            // Guard: inventory must be active
            if (inventory.getIsActive() == false) {
                errors.add(
                        localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_DISABLED.getMessage()));
            }
        }

        // Get current authenticated user
        try {
            user = SecurityUtils.getCurrentUser();
            if (user.getStatus() != UserStatus.ACTIVE) {
                errors.add(localizedErrorService.getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()));
            }

            // Check if user has access to the organization (user must be the creator of the
            // organization)
            if (inventory != null && !user.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        InventoryErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
            }
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(
                    InventoryErrorCode.USER_NOT_AUTHENTICATED.getMessage()) + ": " + e.getMessage());
        }

        // Normalize string inputs

        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());

        // Friendly uniqueness checks within inventory scope
        if (inventory != null) {

            if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
                try {
                    if (inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(),
                            request.getInventoryId())) {
                        errors.add(localizedErrorService.getLocalizedMessage(
                                InventoryErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + " '" + request.getBarcode()
                                + "'");
                    }
                } catch (Exception e) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
                }
            }

            if (request.getProductCode() != null && !request.getProductCode().trim().isEmpty()) {
                try {
                    if (inventoryItemRepository.existsByProductCodeAndInventoryId(request.getProductCode(),
                            request.getInventoryId())) {
                        errors.add(localizedErrorService
                                .getLocalizedMessage(InventoryErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + " '"
                                + request.getProductCode()
                                + "'");
                    }
                } catch (Exception e) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
                }
            }
        }

        // Cross-field rules
        Integer minStock = request.getMinimumStock();
        Integer maxStock = request.getMaximumStock();
        Integer currentStock = request.getCurrentStock();
        if (maxStock != null && minStock != null && maxStock < minStock) {
            errors.add(
                    localizedErrorService.getLocalizedMessage(InventoryErrorCode.MAX_STOCK_LESS_THAN_MIN.getMessage()));
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.CURRENT_STOCK_EXCEEDS_MAX.getMessage()));
        }

        Money costPrice = request.getCostPrice();
        Money sellingPrice = request.getSellingPrice();

        // Validate currency is provided
        if (sellingPrice != null && sellingPrice.getCurrency() == null) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.CURRENCY_REQUIRED_FOR_SELLING_PRICE.getMessage()));
        }
        if (costPrice != null && costPrice.getCurrency() == null) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.CURRENCY_REQUIRED_FOR_COST_PRICE.getMessage()));
        }

        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.SELLING_PRICE_LESS_THAN_COST.getMessage()));
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        if (discountPrice != null) {
            if (sellingPrice == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.SELLING_PRICE_REQUIRED_FOR_DISCOUNT.getMessage()));
            }
            if (discountPrice.compareTo(sellingPrice.getAmount()) > 0) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.DISCOUNT_PRICE_EXCEEDS_SELLING.getMessage()));
            }
            if (discountStart == null || discountEnd == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.DISCOUNT_DATES_REQUIRED.getMessage()));
            }
            if (discountStart != null && discountEnd != null && discountStart.isAfter(discountEnd)) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.DISCOUNT_START_AFTER_END.getMessage()));
            }
        }

        Boolean isPerishable = request.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate();
        if (Boolean.TRUE.equals(isPerishable)) {
            if (expiryDate == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.EXPIRY_DATE_REQUIRED_FOR_PERISHABLE.getMessage()));
            } else if (!expiryDate.isAfter(LocalDate.now())) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.EXPIRY_DATE_MUST_BE_FUTURE.getMessage()));
            }
        } else if (Boolean.FALSE.equals(isPerishable)) {
            // For non-perishable items, expiry date should be null or empty
            // Allow empty string from CSV to be treated as null
            if (expiryDate != null) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        InventoryErrorCode.EXPIRY_DATE_MUST_BE_NULL_FOR_NON_PERISHABLE.getMessage()));
            }
        }
        // If isPerishable is null (not specified), don't enforce expiry date rules

        return new ValidationResult(errors, inventory, user);
    }

    /**
     * Original validate method for backward compatibility (throws exceptions)
     */
    public ValidatedCreateInventoryItemContext validate(CreateInventoryItemRequest request) {
        ValidationResult result = validateAndCollectErrors(request);

        if (result.hasErrors()) {
            // For backward compatibility, throw the first error
            throw new BadRequestException(result.getErrors().get(0));
        }

        return new ValidatedCreateInventoryItemContext(result.getInventory(), result.getUser());
    }
}
