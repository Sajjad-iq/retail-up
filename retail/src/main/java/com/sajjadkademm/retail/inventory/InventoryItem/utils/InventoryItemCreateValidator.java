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

@Component
@RequiredArgsConstructor
public class InventoryItemCreateValidator {

    private final InventoryService inventoryService;
    private final UserService userService;
    private final OrganizationService organizationService;
    private final InventoryItemRepository inventoryItemRepository;

    public ValidatedCreateInventoryItemContext validate(CreateInventoryItemRequest request) {
        // Resolve and validate inventory
        Inventory inventory = inventoryService.getInventoryById(request.getInventoryId());
        if (inventory == null) {
            throw new NotFoundException("Inventory not found");
        }

        // name is required
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            throw new BadRequestException("Name is required");
        }

        // unit is required
        if (request.getUnit() == null) {
            throw new BadRequestException("Unit is required");
        }

        // current stock must be greater than or equal to 0
        // current stock is required
        if (request.getCurrentStock() == null || request.getCurrentStock() < 0) {
            throw new BadRequestException("Current stock cannot be negative");
        }
        // minimum stock must be greater than or equal to 0
        if (request.getMinimumStock() != null && request.getMinimumStock() < 0) {
            throw new BadRequestException("Minimum stock cannot be negative");
        }

        Organization organization = organizationService.getOrganizationById(inventory.getOrganizationId());
        if (organization == null) {
            throw new NotFoundException("Organization not found");
        }
        OrganizationValidationUtils.assertOrganizationIsActive(organization);

        // Guard: inventory must be active
        if (inventory.getIsActive() == false) {
            throw new BadRequestException("This Inventory Disabled");
        }

        // Resolve creating user
        User user = userService.getUserById(request.getUserId());
        if (user == null) {
            throw new NotFoundException("User not found");
        }

        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException("Only Active Users Can Create Inventory Items");
        }

        // Normalize string inputs
        if (request.getSku() != null)
            request.setSku(request.getSku().trim());
        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());

        // Friendly uniqueness checks within inventory scope
        if (request.getSku() != null && !request.getSku().trim().isEmpty()) {
            if (inventoryItemRepository.existsBySkuAndInventoryId(request.getSku(), request.getInventoryId())) {
                throw new ConflictException(
                        "Item with SKU '" + request.getSku() + "' already exists in this inventory");
            }
        }
        if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
            if (inventoryItemRepository.existsByBarcodeAndInventoryId(request.getBarcode(), request.getInventoryId())) {
                throw new ConflictException(
                        "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
            }
        }
        if (request.getProductCode() != null && !request.getProductCode().trim().isEmpty()) {
            if (inventoryItemRepository.existsByProductCodeAndInventoryId(request.getProductCode(),
                    request.getInventoryId())) {
                throw new ConflictException(
                        "Item with product code '" + request.getProductCode() + "' already exists in this inventory");
            }
        }

        // Cross-field rules
        Integer minStock = request.getMinimumStock();
        Integer maxStock = request.getMaximumStock();
        Integer currentStock = request.getCurrentStock();
        if (maxStock != null && minStock != null && maxStock < minStock) {
            throw new BadRequestException("Maximum stock cannot be less than minimum stock");
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            throw new BadRequestException("Current stock cannot exceed maximum stock");
        }

        Money costPrice = request.getCostPrice();
        Money sellingPrice = request.getSellingPrice();

        // Validate currency is provided
        if (sellingPrice != null && sellingPrice.getCurrency() == null) {
            throw new BadRequestException("Currency is required for selling price");
        }
        if (costPrice != null && costPrice.getCurrency() == null) {
            throw new BadRequestException("Currency is required for cost price");
        }

        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            throw new BadRequestException("Selling price cannot be less than cost price");
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        if (discountPrice != null) {
            if (sellingPrice == null) {
                throw new BadRequestException("Selling price is required when discount price is provided");
            }
            if (discountPrice.compareTo(sellingPrice.getAmount()) > 0) {
                throw new BadRequestException("Discount price cannot exceed selling price");
            }
            if (discountStart == null || discountEnd == null) {
                throw new BadRequestException(
                        "Discount start and end dates are required when discount price is provided");
            }
            if (discountStart.isAfter(discountEnd)) {
                throw new BadRequestException("Discount start date cannot be after discount end date");
            }
        }

        Boolean isPerishable = request.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate();
        if (Boolean.TRUE.equals(isPerishable)) {
            if (expiryDate == null) {
                throw new BadRequestException("Expiry date is required for perishable items");
            }
            if (!expiryDate.isAfter(LocalDate.now())) {
                throw new BadRequestException("Expiry date must be in the future for perishable items");
            }
        } else if (Boolean.FALSE.equals(isPerishable)) {
            // For non-perishable items, expiry date should be null or empty
            // Allow empty string from CSV to be treated as null
            if (expiryDate != null) {
                throw new BadRequestException("Expiry date must be null for non-perishable items");
            }
        }
        // If isPerishable is null (not specified), don't enforce expiry date rules

        // Hand off validated dependencies
        return new ValidatedCreateInventoryItemContext(inventory, user);
    }
}
