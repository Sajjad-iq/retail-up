package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.shared.enums.Unit;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateInventoryItemRequest {

    // temporary id required for excelUpload module to update existing items
    // not actually needed for creation
    private String id;

    @NotBlank(message = "{inventory.id.required}")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "{inventory.id.invalid}")
    @Size(min = 20, max = 255, message = "{inventory.id.invalid}")
    private String inventoryId;

    // Basic Product Information
    @NotBlank(message = "{name.required}")
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

    @NotNull(message = "{unit.required}")
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
    @NotNull(message = "{inventoryItem.current.stock.required}")
    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer currentStock = 0;

    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer minimumStock;

    @Min(value = 0, message = "{stock.cannot.be.negative}")
    private Integer maximumStock;

    // Pricing Information
    private Money costPrice;

    @NotNull(message = "Selling price is required")
    private Money sellingPrice;

    @DecimalMin(value = "0.0", message = "{inventoryItem.discount.price.negative}")
    private BigDecimal discountPrice;

    private LocalDateTime discountStartDate;

    private LocalDateTime discountEndDate;

    // Supplier name for quick reference without joining tables
    @Size(max = 200, message = "{inventoryItem.supplier.name.too.long}")
    private String supplierName;

    // Expiry and Perishability
    // Whether the product has an expiration date (food, medicine, etc.)
    private Boolean isPerishable = false;

    // Expiry date for perishable items
    private LocalDate expiryDate;
}