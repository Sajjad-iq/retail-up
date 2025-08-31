package com.sajjadkademm.retail.inventory.InventoryItem.validator;

import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.shared.validators.OrganizationValidator;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.users.User;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Shared validation utilities for inventory item operations
 */
@Component
@RequiredArgsConstructor
public class InventoryItemValidationUtils {

    private final InventoryService inventoryService;
    private final OrganizationService organizationService;
    private final InventoryItemRepository inventoryItemRepository;
    private final LocalizedErrorService localizedErrorService;
    private final OrganizationValidator organizationValidationUtils;
    private final UserValidator userValidator;

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
     * Validate and resolve inventory from inventory ID
     */
    public Inventory validateAndResolveInventory(String inventoryId, List<String> errors) {
        try {
            Inventory inventory = inventoryService.getInventoryById(inventoryId);
            if (inventory == null) {
                errors.add(
                        localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage()));
                return null;
            }

            // Guard: inventory must be active
            if (Boolean.FALSE.equals(inventory.getIsActive())) {
                errors.add(
                        localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_INACTIVE.getMessage()));
            }

            return inventory;
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage())
                    + ": " + e.getMessage());
            return null;
        }
    }

    /**
     * Validate basic required fields
     */
    public void validateBasicFields(String name, Object unit, Integer currentStock, Integer minimumStock,
            List<String> errors) {
        // name is required
        if (name == null || name.trim().isEmpty()) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryItemErrorCode.NAME_REQUIRED.getMessage()));
        }

        // unit is required
        if (unit == null) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryItemErrorCode.UNIT_REQUIRED.getMessage()));
        }

        // current stock must be greater than or equal to 0
        if (currentStock == null || currentStock < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }

        // minimum stock must be greater than or equal to 0
        if (minimumStock != null && minimumStock < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }
    }

    /**
     * Validate organization and ensure it is active
     */
    public void validateOrganization(Inventory inventory, List<String> errors) {
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
        }
    }

    /**
     * Validate user and ensure it is active
     * Gets user ID from SecurityUtils and fetches actual user from database
     */
    public User validateUser(List<String> errors) {
        try {
            String userId = SecurityUtils.getCurrentUserId();
            User user = userValidator.validateUserActive(userId);
            return user;
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()) + ": "
                    + e.getMessage());
            return null;
        }
    }

    /**
     * Validate user has organization creator access (for create operations)
     */
    public void validateOrganizationCreatorAccess(User user, Inventory inventory, List<String> errors) {
        if (user != null && inventory != null
                && !user.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Normalize string inputs
     */
    public void normalizeStringInputs(String[] barcode, String[] productCode) {
        if (barcode[0] != null) {
            barcode[0] = barcode[0].trim();
        }
        if (productCode[0] != null) {
            productCode[0] = productCode[0].trim();
        }
    }

    /**
     * Validate barcode uniqueness within inventory scope
     */
    public void validateBarcodeUniqueness(String barcode, String inventoryId, String existingBarcode, boolean isUpdate,
            List<String> errors) {
        if (barcode != null && !barcode.trim().isEmpty()) {
            try {
                boolean shouldCheck = isUpdate ? !barcode.equals(existingBarcode) : true;
                if (shouldCheck && inventoryItemRepository.existsByBarcodeAndInventoryId(barcode, inventoryId)) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryItemErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + " '" + barcode + "'");
                }
            } catch (Exception e) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
            }
        }
    }

    /**
     * Validate product code uniqueness within inventory scope
     */
    public void validateProductCodeUniqueness(String productCode, String inventoryId, String existingProductCode,
            boolean isUpdate, List<String> errors) {
        if (productCode != null && !productCode.trim().isEmpty()) {
            try {
                boolean shouldCheck = isUpdate ? !productCode.equals(existingProductCode) : true;
                if (shouldCheck
                        && inventoryItemRepository.existsByProductCodeAndInventoryId(productCode, inventoryId)) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryItemErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + " '" + productCode
                            + "'");
                }
            } catch (Exception e) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
            }
        }
    }

    /**
     * Validate stock cross-field rules
     */
    public void validateStockRules(Integer minStock, Integer maxStock, Integer currentStock, List<String> errors) {
        if (maxStock != null && minStock != null && maxStock < minStock) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.MAX_STOCK_LESS_THAN_MIN.getMessage()));
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.CURRENT_STOCK_EXCEEDS_MAX.getMessage()));
        }
    }

    /**
     * Validate currency requirements for pricing
     */
    public void validateCurrencyRequirements(Money costPrice, Money sellingPrice, boolean isUpdate,
            List<String> errors) {
        // Validate currency is provided when updating pricing
        if (isUpdate) {
            if (costPrice != null && costPrice.getCurrency() == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_COST_PRICE.getMessage()));
            }
            if (sellingPrice != null && sellingPrice.getCurrency() == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_SELLING_PRICE.getMessage()));
            }
        } else {
            // For create operations, validate currency is provided
            if (sellingPrice != null && sellingPrice.getCurrency() == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_SELLING_PRICE.getMessage()));
            }
            if (costPrice != null && costPrice.getCurrency() == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_COST_PRICE.getMessage()));
            }
        }
    }

    /**
     * Validate price relationships
     */
    public void validatePriceRelationships(Money costPrice, Money sellingPrice, List<String> errors) {
        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.SELLING_PRICE_LESS_THAN_COST.getMessage()));
        }
    }

    /**
     * Validate discount rules
     */
    public void validateDiscountRules(BigDecimal discountPrice, LocalDateTime discountStart, LocalDateTime discountEnd,
            Money sellingPrice, boolean isUpdate, List<String> errors) {

        // For update operations, only enforce discount-related rules if any discount
        // field is being changed
        if (isUpdate && discountPrice == null && discountStart == null && discountEnd == null) {
            return;
        }

        if (discountPrice != null) {
            BigDecimal effectiveSelling = sellingPrice != null ? sellingPrice.getAmount() : null;

            if (effectiveSelling == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.SELLING_PRICE_REQUIRED_FOR_DISCOUNT.getMessage()));
            } else if (discountPrice.compareTo(effectiveSelling) > 0) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.DISCOUNT_PRICE_EXCEEDS_SELLING.getMessage()));
            }

            if (discountStart == null || discountEnd == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.DISCOUNT_DATES_REQUIRED.getMessage()));
            }

            if (discountStart != null && discountEnd != null && discountStart.isAfter(discountEnd)) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.DISCOUNT_START_AFTER_END.getMessage()));
            }
        }
    }

    /**
     * Validate perishable item rules
     */
    public void validatePerishableRules(Boolean isPerishable, LocalDate expiryDate, List<String> errors) {
        if (Boolean.TRUE.equals(isPerishable)) {
            if (expiryDate == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.EXPIRY_DATE_REQUIRED_FOR_PERISHABLE.getMessage()));
            } else if (!expiryDate.isAfter(LocalDate.now())) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.EXPIRY_DATE_MUST_BE_FUTURE.getMessage()));
            }
        } else if (Boolean.FALSE.equals(isPerishable)) {
            if (expiryDate != null) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        InventoryItemErrorCode.EXPIRY_DATE_MUST_BE_NULL_FOR_NON_PERISHABLE.getMessage()));
            }
        }
    }

    // Security helper methods (moved from InventoryItemService)

    /**
     * Check if current user has access to the inventory item
     * Gets user ID from SecurityUtils and validates access through organization
     * creator check
     */
    public void checkUserAccessToInventoryItem(InventoryItem item) {
        try {
            // Get current authenticated user ID and fetch from database
            String userId = SecurityUtils.getCurrentUserId();
            User currentUser = userValidator.validateUserActive(userId);

            // Get inventory and check if user has access to it
            Inventory inventory = inventoryService.getInventoryById(item.getInventoryId());
            if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
                throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                        UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
            }
        } catch (UnauthorizedException e) {
            throw e; // Re-throw authorization exceptions
        } catch (Exception e) {
            throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                    UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()) + ": " + e.getMessage());
        }
    }

    /**
     * Check if current user has access to the inventory
     * Gets user ID from SecurityUtils and validates access through organization
     * creator check
     */
    public void checkUserAccessToInventory(String inventoryId) {
        try {
            // Get current authenticated user ID and fetch from database
            String userId = SecurityUtils.getCurrentUserId();
            User currentUser = userValidator.validateUserActive(userId);

            // Get inventory and check access
            Inventory inventory = inventoryService.getInventoryById(inventoryId);
            if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
                throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                        UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
            }
        } catch (UnauthorizedException e) {
            throw e; // Re-throw authorization exceptions
        } catch (Exception e) {
            throw new UnauthorizedException(localizedErrorService.getLocalizedMessage(
                    UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()) + ": " + e.getMessage());
        }
    }

    /**
     * Validate user access to inventory item with error collection (non-throwing
     * version)
     * Adds access validation errors to the provided error list instead of throwing
     * exceptions
     */
    public void validateUserAccessToInventoryItem(InventoryItem item, List<String> errors) {
        try {
            checkUserAccessToInventoryItem(item);
        } catch (Exception e) {
            errors.add(e.getMessage());
        }
    }

    /**
     * Validate user access to inventory with error collection (non-throwing
     * version)
     * Adds access validation errors to the provided error list instead of throwing
     * exceptions
     */
    public void validateUserAccessToInventory(String inventoryId, List<String> errors) {
        try {
            checkUserAccessToInventory(inventoryId);
        } catch (Exception e) {
            errors.add(e.getMessage());
        }
    }
}
