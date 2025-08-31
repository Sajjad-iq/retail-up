package com.sajjadkademm.retail.inventory.InventoryItem.dto;

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
    @Size(min = 2, max = 200, message = "{inventory.name.invalid}")
    private String name;

    @Size(max = 1000, message = "{inventory.description.invalid}")
    private String description;

    @Size(max = 300, message = "{inventoryItem.product.code.too.long}")
    private String productCode;

    // Product Identification
    @Size(max = 300, message = "{inventoryItem.barcode.too.long}")
    private String barcode;

    // Product Classification
    @Size(max = 200, message = "{inventoryItem.category.too.long}")
    private String category;

    @Size(max = 200, message = "{inventoryItem.brand.too.long}")
    private String brand;

    private Unit unit;

    // Physical Attributes
    @DecimalMin(value = "0.0", message = "{inventoryItem.weight.negative}")
    private BigDecimal weight;

    @Size(max = 50, message = "{inventoryItem.dimensions.too.long}")
    private String dimensions;

    // Product Variants
    @Size(max = 50, message = "{inventoryItem.color.too.long}")
    private String color;

    @Size(max = 20, message = "{inventoryItem.size.too.long}")
    private String size;

    // Stock Management
    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer currentStock;

    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer minimumStock;

    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer maximumStock;

    // Pricing Information
    private Money costPrice;

    @NotNull(message = "{inventoryItem.selling.price.required}")
    private Money sellingPrice;

    @DecimalMin(value = "0.0", message = "{inventoryItem.discount.price.negative}")
    private BigDecimal discountPrice;

    private LocalDateTime discountStartDate;

    private LocalDateTime discountEndDate;

    @Size(max = 200, message = "{inventoryItem.supplier.name.too.long}")
    private String supplierName;

    // Expiry and Perishability
    private Boolean isPerishable;

    private LocalDate expiryDate;

    // Status
    private Boolean isActive = true;
}