package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateInventoryItemRequest {

    // Basic Product Information
    @Size(min = 2, max = 200, message = "Item name must be between 2 and 200 characters")
    private String name;

    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;

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

    @Size(max = 50, message = "Location must not exceed 50 characters")
    private String location;

    // Stock Management
    @Min(value = 0, message = "Current stock cannot be negative")
    private Integer currentStock;

    @Min(value = 0, message = "Minimum stock cannot be negative")
    private Integer minimumStock;

    @Min(value = 0, message = "Maximum stock cannot be negative")
    private Integer maximumStock;

    // Pricing Information
    @DecimalMin(value = "0.0", message = "Cost price cannot be negative")
    private BigDecimal costPrice;

    @DecimalMin(value = "0.0", message = "Selling price cannot be negative")
    private BigDecimal sellingPrice;

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
    private Boolean isActive;
}