package com.sajjadkademm.retail.inventory.InventoryItem.validator;

import com.sajjadkademm.retail.config.locales.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.shared.validators.OrganizationValidator;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.config.locales.errorCode.InventoryItemErrorCode;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.ReferenceType;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class InventoryItemUpdateValidator {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryService inventoryService;
    private final OrganizationService organizationService;
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;
    private final OrganizationValidator organizationValidationUtils;

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
     * Validate update request and collect all errors instead of throwing exceptions
     */
    public ValidationResult validateAndCollectErrors(InventoryItem existing, UpdateInventoryItemRequest request) {
        List<String> errors = new ArrayList<>();
        Inventory inventory = null;
        User user = null;

        try {
            // Resolve inventory and guard it exists
            inventory = inventoryService.getInventoryById(existing.getInventoryId());
            if (inventory == null) {
                errors.add(
                        localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage()));
            } else {
                // Guard: inventory must be active
                if (Boolean.FALSE.equals(inventory.getIsActive())) {
                    errors.add(localizedErrorService
                            .getLocalizedMessage(InventoryErrorCode.INVENTORY_INACTIVE.getMessage()));
                }
            }
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage())
                    + ": " + e.getMessage());
        }

        // name is required
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryItemErrorCode.NAME_REQUIRED.getMessage()));
        }

        // unit is required
        if (request.getUnit() == null) {
            errors.add(localizedErrorService.getLocalizedMessage(InventoryItemErrorCode.UNIT_REQUIRED.getMessage()));
        }

        // current stock must be greater than or equal to 0
        if (request.getCurrentStock() == null || request.getCurrentStock() < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }

        // minimum stock must be greater than or equal to 0
        if (request.getMinimumStock() != null && request.getMinimumStock() < 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.STOCK_CANNOT_BE_NEGATIVE.getMessage()));
        }

        // Resolve organization and ensure it is active
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
                        errors.add(
                                localizedErrorService.getLocalizedMessage(
                                        OrganizationErrorCode.ORGANIZATION_VALIDATION_FAILED.getMessage()) + ": "
                                        + e.getMessage());
                    }
                }
            } catch (Exception e) {
                errors.add(localizedErrorService.getLocalizedMessage(
                        OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()) + ": " + e.getMessage());
            }
        }

        // Resolve user and ensure it is active
        try {
            user = SecurityUtils.getCurrentUser();
            if (user.getStatus() != UserStatus.ACTIVE) {
                errors.add(localizedErrorService.getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()));
            }
        } catch (Exception e) {
            errors.add(localizedErrorService.getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()) + ": "
                    + e.getMessage());
        }

        // Normalize string inputs
        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());

        // Friendly uniqueness checks within inventory scope (only when changed)
        if (inventory != null) {

            if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
                try {
                    boolean isChangingBarcode = !request.getBarcode().equals(existing.getBarcode());
                    if (isChangingBarcode && inventoryItemRepository
                            .existsByBarcodeAndInventoryId(request.getBarcode(), existing.getInventoryId())) {
                        errors.add(localizedErrorService.getLocalizedMessage(
                                InventoryItemErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + " '" + request.getBarcode()
                                + "'");
                    }
                } catch (Exception e) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryItemErrorCode.BARCODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
                }
            }

            if (request.getProductCode() != null && !request.getProductCode().trim().isEmpty()) {
                try {
                    boolean isChangingProductCode = !request.getProductCode().equals(existing.getProductCode());
                    if (isChangingProductCode && inventoryItemRepository
                            .existsByProductCodeAndInventoryId(request.getProductCode(), existing.getInventoryId())) {
                        errors.add(localizedErrorService
                                .getLocalizedMessage(InventoryItemErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + " '"
                                + request.getProductCode()
                                + "'");
                    }
                } catch (Exception e) {
                    errors.add(localizedErrorService.getLocalizedMessage(
                            InventoryItemErrorCode.PRODUCT_CODE_ALREADY_EXISTS.getMessage()) + ": " + e.getMessage());
                }
            }
        }

        // Cross-field rules (use new value if present, otherwise existing)
        Integer minStock = request.getMinimumStock() != null ? request.getMinimumStock() : existing.getMinimumStock();
        Integer maxStock = request.getMaximumStock() != null ? request.getMaximumStock() : existing.getMaximumStock();
        Integer currentStock = request.getCurrentStock() != null ? request.getCurrentStock()
                : existing.getCurrentStock();
        if (maxStock != null && minStock != null && maxStock < minStock) {
            errors.add(
                    localizedErrorService.getLocalizedMessage(InventoryItemErrorCode.MAX_STOCK_LESS_THAN_MIN.getMessage()));
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.CURRENT_STOCK_EXCEEDS_MAX.getMessage()));
        }

        Money costPrice = request.getCostPrice() != null ? request.getCostPrice()
                : existing.getCostPrice();
        Money sellingPrice = request.getSellingPrice() != null ? request.getSellingPrice()
                : existing.getSellingPrice();

        // Validate currency is provided when updating pricing
        if (request.getCostPrice() != null && request.getCostPrice().getCurrency() == null) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_COST_PRICE.getMessage()));
        }
        if (request.getSellingPrice() != null && request.getSellingPrice().getCurrency() == null) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.CURRENCY_REQUIRED_FOR_SELLING_PRICE.getMessage()));
        }

        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            errors.add(localizedErrorService
                    .getLocalizedMessage(InventoryItemErrorCode.SELLING_PRICE_LESS_THAN_COST.getMessage()));
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        // Only enforce discount-related rules if any discount field is being changed
        if (discountPrice != null || discountStart != null || discountEnd != null) {
            BigDecimal effectiveSelling = sellingPrice != null ? sellingPrice.getAmount() : null; // may be from request
                                                                                                  // or existing
            if (discountPrice != null && effectiveSelling == null) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.SELLING_PRICE_REQUIRED_FOR_DISCOUNT.getMessage()));
            }
            if (discountPrice != null && effectiveSelling != null && discountPrice.compareTo(effectiveSelling) > 0) {
                errors.add(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.DISCOUNT_PRICE_EXCEEDS_SELLING.getMessage()));
            }
            if (discountPrice != null) {
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

        Boolean isPerishable = request.getIsPerishable() != null ? request.getIsPerishable()
                : existing.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate() != null ? request.getExpiryDate() : existing.getExpiryDate();
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
