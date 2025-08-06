package com.sajjadkademm.retail.inventory.InventoryItem;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Enumerated;
import jakarta.persistence.EnumType;

import java.time.LocalDateTime;
import java.time.LocalDate;
import java.math.BigDecimal;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.DecimalMax;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;
import com.sajjadkademm.retail.users.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "inventory_items", indexes = {
        @Index(name = "idx_inventory_items_name", columnList = "name"),
        @Index(name = "idx_inventory_items_sku", columnList = "sku"),
        @Index(name = "idx_inventory_items_barcode", columnList = "barcode"),
        @Index(name = "idx_inventory_items_inventory_id", columnList = "inventory_id"),
        @Index(name = "idx_inventory_items_sku_inventory", columnList = "sku,inventory_id"),
        @Index(name = "idx_inventory_items_barcode_inventory", columnList = "barcode,inventory_id"),
        @Index(name = "idx_inventory_items_product_code", columnList = "product_code"),
        @Index(name = "idx_inventory_items_category", columnList = "category"),
        @Index(name = "idx_inventory_items_brand", columnList = "brand"),
        @Index(name = "idx_inventory_items_supplier_id", columnList = "supplier_id"),
        @Index(name = "idx_inventory_items_is_active", columnList = "is_active"),
        @Index(name = "idx_inventory_items_expiry_date", columnList = "expiry_date"),
        @Index(name = "idx_inventory_items_low_stock", columnList = "current_stock,minimum_stock")
})
public class InventoryItem {
    // Primary Key - Unique identifier for each inventory item
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    // Basic Product Information
    // Product name as displayed to customers and in the system
    @Column(name = "name", nullable = false)
    @NotBlank
    @Size(min = 2, max = 200)
    private String name;

    // Detailed product description for customer information and internal reference
    @Column(name = "description", length = 1000)
    @Size(max = 1000)
    private String description;

    // Stock Keeping Unit - Unique identifier for inventory tracking and management
    @Column(name = "sku", nullable = false)
    @NotBlank
    @Size(min = 3, max = 50)
    private String sku;

    // Internal product code for business operations (different from SKU)
    @Column(name = "product_code", length = 50)
    @Size(max = 50)
    private String productCode;

    // Product Identification
    // Barcode for POS scanning and quick identification
    @Column(name = "barcode", length = 100)
    @Size(max = 100)
    private String barcode;

    // Product Classification
    // Category for organizing products (e.g., Electronics, Clothing, Food)
    @Column(name = "category", length = 100)
    @Size(max = 100)
    private String category;

    // Brand name for filtering and customer preference
    @Column(name = "brand", length = 100)
    @Size(max = 100)
    private String brand;

    // Measurement unit for the product (pieces, kg, liters, etc.)
    @Column(name = "unit", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull
    private Unit unit;

    // Physical Attributes
    // Product weight in grams for shipping calculations and specifications
    @Column(name = "weight", precision = 8, scale = 2)
    @DecimalMin(value = "0.0", message = "Weight cannot be negative")
    private BigDecimal weight;

    // Product dimensions in format "Length x Width x Height" (in cm)
    @Column(name = "dimensions", length = 50)
    @Size(max = 50)
    private String dimensions;

    // Product Variants
    // Color variant for products available in multiple colors
    @Column(name = "color", length = 50)
    @Size(max = 50)
    private String color;

    // Size variant for clothing, shoes, etc. (S, M, L, XL, or numeric sizes)
    @Column(name = "size", length = 20)
    @Size(max = 20)
    private String size;

    // Storage location within warehouse/store (e.g., "A1-B2-C3" for
    // Aisle-Shelf-Position)
    @Column(name = "location", length = 50)
    @Size(max = 50)
    private String location;

    // Stock Management
    // Current available stock quantity
    @Column(name = "current_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "Current stock cannot be negative")
    private Integer currentStock;


    // Minimum stock level before reordering alert
    @Column(name = "minimum_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "Minimum stock cannot be negative")
    private Integer minimumStock;

    // Maximum stock capacity for storage optimization
    @Column(name = "maximum_stock")
    @Min(value = 0, message = "Maximum stock cannot be negative")
    private Integer maximumStock;

    // Pricing Information
    // Cost price paid to supplier (for profit calculation)
    @Column(name = "cost_price", precision = 10, scale = 2)
    @DecimalMin(value = "0.0", message = "Cost price cannot be negative")
    private BigDecimal costPrice;

    // Regular selling price to customers
    @Column(name = "selling_price", precision = 10, scale = 2, nullable = false)
    @NotNull
    @DecimalMin(value = "0.0", message = "Selling price cannot be negative")
    private BigDecimal sellingPrice;

    // Discounted price during promotions or sales
    @Column(name = "discount_price", precision = 10, scale = 2)
    @DecimalMin(value = "0.0", message = "Discount price cannot be negative")
    private BigDecimal discountPrice;

    // Start date for discount pricing
    @Column(name = "discount_start_date")
    private LocalDateTime discountStartDate;

    // End date for discount pricing
    @Column(name = "discount_end_date")
    private LocalDateTime discountEndDate;

    // Supplier name for quick reference without joining tables
    @Column(name = "supplier_name", length = 200)
    @Size(max = 200)
    private String supplierName;

    // Expiry and Perishability
    // Whether the product has an expiration date (food, medicine, etc.)
    @Column(name = "is_perishable", nullable = false)
    @NotNull
    @Builder.Default
    private Boolean isPerishable = false;

    // Expiry date for perishable items
    @Column(name = "expiry_date")
    private LocalDate expiryDate;

    // Whether the item is active in the system (soft delete)
    @Column(name = "is_active", nullable = false)
    @NotNull
    @Builder.Default
    private Boolean isActive = true;

    // Sales Analytics
    // Total quantity sold since product creation (lifetime sales)
    @Column(name = "total_sold", nullable = false)
    @NotNull
    @Min(value = 0, message = "Total sold cannot be negative")
    @Builder.Default
    private Integer totalSold = 0;

    // Total revenue generated from this product (lifetime revenue)
    @Column(name = "total_revenue", precision = 15, scale = 2, nullable = false)
    @NotNull
    @DecimalMin(value = "0.0", message = "Total revenue cannot be negative")
    @Builder.Default
    private BigDecimal totalRevenue = BigDecimal.ZERO;

    // Date when the product was last sold
    @Column(name = "last_sold_date")
    private LocalDateTime lastSoldDate;

    // Inventory Association
    // Foreign key reference to the inventory this item belongs to
    @Column(name = "inventory_id", nullable = false)
    @NotNull(message = "Inventory ID is required")
    private String inventoryId;

    // Audit Trail
    // Timestamp when the record was created (auto-generated)
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    // Timestamp when the record was last updated (auto-updated)
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    // Entity Relationships
    // Many-to-One relationship with Inventory entity (multiple items belong to one
    // inventory)
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "inventory_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_items_inventory"))
    private Inventory inventory;

    // Many-to-One relationship with User entity (tracks who created this item)
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}