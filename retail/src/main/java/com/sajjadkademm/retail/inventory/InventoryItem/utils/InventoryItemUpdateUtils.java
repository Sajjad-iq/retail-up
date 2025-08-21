package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.organizations.OrganizationValidationUtils;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class InventoryItemUpdateUtils {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryService inventoryService;
    private final OrganizationService organizationService;
    private final UserRepository userRepository;

    public void validate(InventoryItem existing, UpdateInventoryItemRequest request) {
        // Resolve inventory and guard it exists
        Inventory inventory = inventoryService.getInventoryById(existing.getInventoryId());
        if (inventory == null) {
            throw new NotFoundException("Inventory not found");
        }
        // Guard: inventory must be active
        if (Boolean.FALSE.equals(inventory.getIsActive())) {
            throw new BadRequestException("This Inventory Disabled");
        }

        // Guard: item must be active for updates
        if (Boolean.FALSE.equals(existing.getIsActive())) {
            throw new BadRequestException("This Item Disabled");
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

        // Resolve organization and ensure it is active
        Organization organization = organizationService.getOrganizationById(inventory.getOrganizationId());
        if (organization == null) {
            throw new NotFoundException("Organization not found");
        }
        OrganizationValidationUtils.assertOrganizationIsActive(organization);

        // Resolve user and ensure it is active
        User user = userRepository.findById(request.getUserId())
                .orElseThrow(() -> new NotFoundException("User not found"));

        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException("Only Active Users Can Update Inventory Items");
        }

        // Normalize string inputs
        if (request.getBarcode() != null)
            request.setBarcode(request.getBarcode().trim());
        if (request.getProductCode() != null)
            request.setProductCode(request.getProductCode().trim());
        if (request.getSku() != null) {
            request.setSku(request.getSku().trim());
        }

        if (request.getSku() != null && !request.getSku().trim().isEmpty()) {
            boolean isChangingSku = !request.getSku().equals(existing.getSku());
            if (isChangingSku && inventoryItemRepository
                    .existsBySkuAndInventoryId(request.getSku(), existing.getInventoryId())) {
                throw new ConflictException(
                        "Item with SKU '" + request.getSku() + "' already exists in this inventory");
            }
        }

        // Friendly uniqueness checks within inventory scope (only when changed)
        if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
            boolean isChangingBarcode = !request.getBarcode().equals(existing.getBarcode());
            if (isChangingBarcode && inventoryItemRepository
                    .existsByBarcodeAndInventoryId(request.getBarcode(), existing.getInventoryId())) {
                throw new ConflictException(
                        "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
            }
        }
        if (request.getProductCode() != null && !request.getProductCode().trim().isEmpty()) {
            boolean isChangingProductCode = !request.getProductCode().equals(existing.getProductCode());
            if (isChangingProductCode && inventoryItemRepository
                    .existsByProductCodeAndInventoryId(request.getProductCode(), existing.getInventoryId())) {
                throw new ConflictException(
                        "Item with product code '" + request.getProductCode() + "' already exists in this inventory");
            }
        }

        // Cross-field rules (use new value if present, otherwise existing)
        Integer minStock = request.getMinimumStock() != null ? request.getMinimumStock() : existing.getMinimumStock();
        Integer maxStock = request.getMaximumStock() != null ? request.getMaximumStock() : existing.getMaximumStock();
        Integer currentStock = request.getCurrentStock() != null ? request.getCurrentStock()
                : existing.getCurrentStock();
        if (maxStock != null && minStock != null && maxStock < minStock) {
            throw new BadRequestException("Maximum stock cannot be less than minimum stock");
        }
        if (maxStock != null && currentStock != null && currentStock > maxStock) {
            throw new BadRequestException("Current stock cannot exceed maximum stock");
        }

        Money costPrice = request.getCostPrice() != null ? request.getCostPrice()
                : existing.getCostPrice();
        Money sellingPrice = request.getSellingPrice() != null ? request.getSellingPrice()
                : existing.getSellingPrice();

        // Validate currency is provided when updating pricing
        if (request.getCostPrice() != null && request.getCostPrice().getCurrency() == null) {
            throw new BadRequestException("Currency is required for cost price");
        }
        if (request.getSellingPrice() != null && request.getSellingPrice().getCurrency() == null) {
            throw new BadRequestException("Currency is required for selling price");
        }

        if (costPrice != null && sellingPrice != null
                && costPrice.getAmount().compareTo(sellingPrice.getAmount()) > 0) {
            throw new BadRequestException("Selling price cannot be less than cost price");
        }

        BigDecimal discountPrice = request.getDiscountPrice();
        LocalDateTime discountStart = request.getDiscountStartDate();
        LocalDateTime discountEnd = request.getDiscountEndDate();
        // Only enforce discount-related rules if any discount field is being changed
        if (discountPrice != null || discountStart != null || discountEnd != null) {
            BigDecimal effectiveSelling = sellingPrice != null ? sellingPrice.getAmount() : null; // may be from request
                                                                                                  // or existing
            if (discountPrice != null && effectiveSelling == null) {
                throw new BadRequestException("Selling price is required when discount price is provided");
            }
            if (discountPrice != null && effectiveSelling != null && discountPrice.compareTo(effectiveSelling) > 0) {
                throw new BadRequestException("Discount price cannot exceed selling price");
            }
            if (discountPrice != null) {
                if (discountStart == null || discountEnd == null) {
                    throw new BadRequestException(
                            "Discount start and end dates are required when discount price is provided");
                }
                if (discountStart.isAfter(discountEnd)) {
                    throw new BadRequestException("Discount start date cannot be after discount end date");
                }
            }
        }

        Boolean isPerishable = request.getIsPerishable() != null ? request.getIsPerishable()
                : existing.getIsPerishable();
        LocalDate expiryDate = request.getExpiryDate() != null ? request.getExpiryDate() : existing.getExpiryDate();
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
        if (request.getSku() != null) {
            item.setSku(request.getSku());
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
        User actor = userRepository.findById(request.getUserId())
                .orElseThrow(() -> new NotFoundException("User not found"));

        if (request.getSku() != null && !request.getSku().equals(item.getSku())) {
            inventoryMovementService.recordAdjustmentToTarget(
                    actor,
                    item,
                    item.getCurrentStock(),
                    "SKU updated via item update",
                    ReferenceType.INFO_UPDATE,
                    item.getId());
        }

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
