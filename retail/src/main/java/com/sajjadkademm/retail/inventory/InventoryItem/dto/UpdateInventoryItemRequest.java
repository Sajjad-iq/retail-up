package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Pattern;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalDate;

import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateInventoryItemRequest {

    @NotBlank(message = "User ID is required")
    private String userId;

    // Basic Product Information
    @Size(min = 2, max = 200, message = "Item name must be between 2 and 200 characters")
    private String name;

    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;

    @Size(max = 300, message = "Product code must not exceed 300 characters")
    private String productCode;

    // Product Identification
    @Size(max = 300, message = "Barcode must not exceed 300 characters")
    private String barcode;

    // Product Classification
    @Size(max = 200, message = "Category must not exceed 200 characters")
    private String category;

    @Size(max = 200, message = "Brand must not exceed 200 characters")
    private String brand;

    private Unit unit;

    // Physical Attributes
    @DecimalMin(value = "0.0", message = "Weight cannot be negative")
    private BigDecimal weight;

    @Size(max = 50, message = "Dimensions must not exceed 50 characters")
    private String dimensions;

    // Product Variants
    @Size(max = 50, message = "Color must not exceed 50 characters")
    private String color;

    @Size(max = 20, message = "Size must not exceed 20 characters")
    private String size;

    // Stock Management
    @Min(value = 0, message = "Current stock cannot be negative")
    private Integer currentStock;

    @Min(value = 0, message = "Minimum stock cannot be negative")
    private Integer minimumStock;

    @Min(value = 0, message = "Maximum stock cannot be negative")
    private Integer maximumStock;

    // Pricing Information
    private Money costPrice;

    @NotNull(message = "Selling price is required")
    private Money sellingPrice;

    @DecimalMin(value = "0.0", message = "Discount price cannot be negative")
    private BigDecimal discountPrice;

    private LocalDateTime discountStartDate;

    private LocalDateTime discountEndDate;

    @Size(max = 200, message = "Supplier name must not exceed 200 characters")
    private String supplierName;

    // Expiry and Perishability
    private Boolean isPerishable;

    private LocalDate expiryDate;

    // Status
    private Boolean isActive = true;
}