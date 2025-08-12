package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;

import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.utils.dto.Currency;

@Component
@RequiredArgsConstructor
public class InventoryItemUpdateUtils {

    private final InventoryItemRepository inventoryItemRepository;
    private final SystemSettingsService systemSettingsService;

    public void validate(InventoryItem existing, UpdateInventoryItemRequest request) {
        // Check if new barcode conflicts with existing item in the same inventory
        if (request.getBarcode() != null && !request.getBarcode().trim().isEmpty()) {
            boolean isChangingBarcode = !request.getBarcode().equals(existing.getBarcode());
            if (isChangingBarcode && inventoryItemRepository
                    .existsByBarcodeAndInventoryId(request.getBarcode(), existing.getInventoryId())) {
                throw new ConflictException(
                        "Item with barcode '" + request.getBarcode() + "' already exists in this inventory");
            }
        }
    }

    public void applyUpdates(InventoryItem item, UpdateInventoryItemRequest request) {
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
            Currency currency = resolveCurrency(item.getInventory().getOrganizationId());
            if (item.getCostPrice() == null) {
                item.setCostPrice(new Money(request.getCostPrice(), currency));
            } else {
                item.getCostPrice().setAmount(request.getCostPrice());
                item.getCostPrice().setCurrency(currency);
            }
        }
        if (request.getSellingPrice() != null) {
            Currency currency = resolveCurrency(item.getInventory().getOrganizationId());
            if (item.getSellingPrice() == null) {
                item.setSellingPrice(new Money(request.getSellingPrice(), currency));
            } else {
                item.getSellingPrice().setAmount(request.getSellingPrice());
                item.getSellingPrice().setCurrency(currency);
            }
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

    private Currency resolveCurrency(String organizationId) {
        try {
            SystemSetting systemSetting = systemSettingsService.getSystemSettings(organizationId);
            String currencyCode = systemSetting.getCurrency();
            if (currencyCode == null || currencyCode.isBlank()) {
                return Currency.USD;
            }
            try {
                return Currency.valueOf(currencyCode.toUpperCase());
            } catch (IllegalArgumentException ex) {
                return Currency.USD;
            }
        } catch (Exception ex) {
            return Currency.USD;
        }
    }
}
