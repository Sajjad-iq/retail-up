package com.sajjadkademm.retail.inventory.InventoryItem.dto;

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

    @NotBlank(message = "User ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "User ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "User ID must be between 20 and 255 characters")
    private String userId;

    @NotBlank(message = "Inventory ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "Inventory ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "Inventory ID must be between 20 and 255 characters")
    private String inventoryId;

    // Basic Product Information
    @NotBlank(message = "Item name is required")
    @Size(min = 2, max = 200, message = "Item name must be between 2 and 200 characters")
    private String name;

    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;

    @NotBlank(message = "SKU is required")
    @Size(min = 3, max = 50, message = "SKU must be between 3 and 50 characters")
    private String sku;

    @Size(max = 50, message = "Product code must not exceed 50 characters")
    private String productCode;

    // Product Identification
    @Size(max = 100, message = "Barcode must not exceed 100 characters")
    private String barcode;

    // Product Classification
    @Size(max = 100, message = "Category must not exceed 100 characters")
    private String category;

    @Size(max = 100, message = "Brand must not exceed 100 characters")
    private String brand;

    @NotNull(message = "Unit is required")
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
    @NotNull(message = "Current stock is required")
    @Min(value = 0, message = "Current stock cannot be negative")
    private Integer currentStock = 0;

    @NotNull(message = "Minimum stock is required")
    @Min(value = 0, message = "Minimum stock cannot be negative")
    private Integer minimumStock = 0;

    @Min(value = 0, message = "Maximum stock cannot be negative")
    private Integer maximumStock;

    // Pricing Information
    @DecimalMin(value = "0.0", message = "Cost price cannot be negative")
    private BigDecimal costPrice;

    @NotNull(message = "Selling price is required")
    @DecimalMin(value = "0.0", message = "Selling price cannot be negative")
    private BigDecimal sellingPrice;

    @DecimalMin(value = "0.0", message = "Discount price cannot be negative")
    private BigDecimal discountPrice;

    private LocalDateTime discountStartDate;

    private LocalDateTime discountEndDate;

    @Size(max = 200, message = "Supplier name must not exceed 200 characters")
    private String supplierName;

    // Expiry and Perishability
    private Boolean isPerishable = false;

    private LocalDate expiryDate;
}