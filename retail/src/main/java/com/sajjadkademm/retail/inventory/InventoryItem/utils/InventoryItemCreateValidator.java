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
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.organizations.OrganizationValidationUtils;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
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
            throw new NotFoundException("Inventory not found with ID: " + request.getInventoryId());
        }

        // Resolve organization, ensure it exists and is active
        Organization organization = organizationService.getOrganizationById(inventory.getOrganizationId());
        if (organization == null) {
            throw new NotFoundException("Organization not found with ID: " + inventory.getOrganizationId());
        }
        OrganizationValidationUtils.assertOrganizationIsActive(organization);

        // Guard: inventory must be active
        if (inventory.getIsActive() == false) {
            throw new BadRequestException("This Inventory Disabled");
        }

        // Resolve creating user
        User user = userService.getUserById(request.getUserId());
        if (user == null) {
            throw new NotFoundException("User not found with ID: " + request.getUserId());
        }

        // Normalize string inputs
        if (request.getSku() != null)
            request.setSku(request.getSku().trim());
        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());

        // Friendly uniqueness checks within inventory scope
        if (inventoryItemRepository.existsBySkuAndInventoryId(request.getSku(), request.getInventoryId())) {
            throw new ConflictException("Item with SKU '" + request.getSku() + "' already exists in this inventory");
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

        BigDecimal costPrice = request.getCostPrice();
        BigDecimal sellingPrice = request.getSellingPrice();
        if (costPrice != null && sellingPrice != null && costPrice.compareTo(sellingPrice) > 0) {
            throw new BadRequestException("Selling price cannot be less than cost price");
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        if (discountPrice != null) {
            if (sellingPrice == null) {
                throw new BadRequestException("Selling price is required when discount price is provided");
            }
            if (discountPrice.compareTo(sellingPrice) > 0) {
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
            if (expiryDate != null) {
                throw new BadRequestException("Expiry date must be null for non-perishable items");
            }
        }

        // Hand off validated dependencies
        return new ValidatedCreateInventoryItemContext(inventory, user);
    }
}
