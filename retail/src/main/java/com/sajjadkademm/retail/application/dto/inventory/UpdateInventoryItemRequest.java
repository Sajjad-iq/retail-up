package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import com.sajjadkademm.retail.shared.enums.Unit;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalDate;

import com.sajjadkademm.retail.shared.enums.Money;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateInventoryItemRequest {

    @NotBlank(message = "{inventoryItem.user.id.required}")
    private String userId;

    // Basic Product Information
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH, message = "{inventory.name.invalid}")
    private String name;

    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH, message = "{inventory.description.invalid}")
    private String description;

    @Size(max = ValidationConstants.MAX_PRODUCT_CODE_LENGTH, message = "{inventoryItem.product.code.too.long}")
    private String productCode;

    // Product Identification
    @Size(max = ValidationConstants.MAX_BARCODE_LENGTH, message = "{inventoryItem.barcode.too.long}")
    private String barcode;

    // Product Classification
    @Size(max = ValidationConstants.MAX_CATEGORY_LENGTH, message = "{inventoryItem.category.too.long}")
    private String category;

    @Size(max = ValidationConstants.MAX_BRAND_LENGTH, message = "{inventoryItem.brand.too.long}")
    private String brand;

    private Unit unit;

    // Physical Attributes
    @DecimalMin(value = "0.0", message = "{inventoryItem.weight.negative}")
    private BigDecimal weight;

    @Size(max = ValidationConstants.MAX_DIMENSIONS_LENGTH, message = "{inventoryItem.dimensions.too.long}")
    private String dimensions;

    // Product Variants
    @Size(max = ValidationConstants.MAX_COLOR_LENGTH, message = "{inventoryItem.color.too.long}")
    private String color;

    @Size(max = ValidationConstants.MAX_SIZE_LENGTH, message = "{inventoryItem.size.too.long}")
    private String size;

    // Stock Management
    @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
    private Integer currentStock;

    @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
    private Integer minimumStock;

    @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
    private Integer maximumStock;

    // Pricing Information
    private Money costPrice;

    @NotNull(message = "{inventoryItem.selling.price.required}")
    private Money sellingPrice;

    @DecimalMin(value = "0.0", message = "{inventoryItem.discount.price.negative}")
    private BigDecimal discountPrice;

    private LocalDateTime discountStartDate;

    private LocalDateTime discountEndDate;

    @Size(max = ValidationConstants.MAX_SUPPLIER_LENGTH, message = "{inventoryItem.supplier.name.too.long}")
    private String supplierName;

    // Expiry and Perishability
    private Boolean isPerishable;

    private LocalDate expiryDate;

    // Status
    private Boolean isActive = true;
}