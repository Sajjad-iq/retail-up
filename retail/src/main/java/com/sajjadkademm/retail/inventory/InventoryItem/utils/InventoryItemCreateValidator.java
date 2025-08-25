package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;

import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.organizations.OrganizationValidationUtils;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class InventoryItemCreateValidator {

    private final InventoryService inventoryService;
    private final UserService userService;
    private final OrganizationService organizationService;
    private final InventoryItemRepository inventoryItemRepository;

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
                errors.add("INV100: Inventory not found");
            }
        } catch (Exception e) {
            errors.add("INV100: Failed to resolve inventory: " + e.getMessage());
        }

        // name is required
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            errors.add("INV013: Name is required");
        }

        // unit is required
        if (request.getUnit() == null) {
            errors.add("INV013: Unit is required");
        }

        // current stock must be greater than or equal to 0
        if (request.getCurrentStock() == null || request.getCurrentStock() < 0) {
            errors.add("INV005: Current stock cannot be negative");
        }

        // minimum stock must be greater than or equal to 0
        if (request.getMinimumStock() != null && request.getMinimumStock() < 0) {
            errors.add("INV005: Minimum stock cannot be negative");
        }

        // Validate organization if inventory was resolved
        if (inventory != null) {
            try {
                Organization organization = organizationService.getOrganizationById(inventory.getOrganizationId());
                if (organization == null) {
                    errors.add("INV102: Organization not found");
                } else {
                    try {
                        OrganizationValidationUtils.assertOrganizationIsActive(organization);
                    } catch (Exception e) {
                        errors.add("INV103: Organization validation failed: " + e.getMessage());
                    }
                }
            } catch (Exception e) {
                errors.add("INV102: Failed to resolve organization: " + e.getMessage());
            }

            // Guard: inventory must be active
            if (inventory.getIsActive() == false) {
                errors.add("INV101: Inventory is disabled");
            }
        }

        // Resolve creating user
        try {
            user = userService.getUserById(request.getUserId());
            if (user == null) {
                errors.add("INV104: User not found");
            } else if (user.getStatus() != UserStatus.ACTIVE) {
                errors.add("INV105: Only Active Users Can Create Inventory Items");
            }
        } catch (Exception e) {
            errors.add("INV104: Failed to resolve user: " + e.getMessage());
        }

        // Normalize string inputs
        if (request.getSku() != null)
            request.setSku(request.getSku().trim());
        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());

        // Friendly uniqueness checks within inventory scope
        if (inventory != null) {
            if (request.getSku() != null && !request.getSku().trim().isEmpty()) {
                try {
                    if (inventoryItemRepository.existsBySkuAndInventoryId(request.getSku(), request.getInventoryId())) {
                        errors.add("INV002: Item with SKU '" + request.getSku() + "' already exists in this inventory");
                    }
                } catch (Exception e) {
                    errors.add("INV002: Failed to check SKU uniqueness: " + e.getMessage());
                }
            }

            if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
                try {
                    if (inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(),
                            request.getInventoryId())) {
                        errors.add("INV003: Item with barcode '" + request.getBarcode()
                                + "' already exists in this inventory");
                    }
                } catch (Exception e) {
                    errors.add("INV003: Failed to check barcode uniqueness: " + e.getMessage());
                }
            }

            if (request.getProductCode() != null && !request.getProductCode().trim().isEmpty()) {
                try {
                    if (inventoryItemRepository.existsByProductCodeAndInventoryId(request.getProductCode(),
                            request.getInventoryId())) {
                        errors.add("INV004: Item with product code '" + request.getProductCode()
                                + "' already exists in this inventory");
                    }
                } catch (Exception e) {
                    errors.add("INV004: Failed to check product code uniqueness: " + e.getMessage());
                }
            }
        }

        // Cross-field rules
        Integer minStock = request.getMinimumStock();
        Integer maxStock = request.getMaximumStock();
        Integer currentStock = request.getCurrentStock();
        if (maxStock != null && minStock != null && maxStock < minStock) {
            errors.add("INV006: Maximum stock cannot be less than minimum stock");
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            errors.add("INV007: Current stock cannot exceed maximum stock");
        }

        Money costPrice = request.getCostPrice();
        Money sellingPrice = request.getSellingPrice();

        // Validate currency is provided
        if (sellingPrice != null && sellingPrice.getCurrency() == null) {
            errors.add("INV015: Currency is required for selling price");
        }
        if (costPrice != null && costPrice.getCurrency() == null) {
            errors.add("INV015: Currency is required for cost price");
        }

        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            errors.add("INV008: Selling price cannot be less than cost price");
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        if (discountPrice != null) {
            if (sellingPrice == null) {
                errors.add("INV009: Selling price is required when discount price is provided");
            }
            if (discountPrice.compareTo(sellingPrice.getAmount()) > 0) {
                errors.add("INV009: Discount price cannot exceed selling price");
            }
            if (discountStart == null || discountEnd == null) {
                errors.add("INV010: Discount start and end dates are required when discount price is provided");
            }
            if (discountStart != null && discountEnd != null && discountStart.isAfter(discountEnd)) {
                errors.add("INV010: Discount start date cannot be after discount end date");
            }
        }

        Boolean isPerishable = request.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate();
        if (Boolean.TRUE.equals(isPerishable)) {
            if (expiryDate == null) {
                errors.add("INV011: Expiry date is required for perishable items");
            } else if (!expiryDate.isAfter(LocalDate.now())) {
                errors.add("INV011: Expiry date must be in the future for perishable items");
            }
        } else if (Boolean.FALSE.equals(isPerishable)) {
            // For non-perishable items, expiry date should be null or empty
            // Allow empty string from CSV to be treated as null
            if (expiryDate != null) {
                errors.add("INV012: Expiry date must be null for non-perishable items");
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
