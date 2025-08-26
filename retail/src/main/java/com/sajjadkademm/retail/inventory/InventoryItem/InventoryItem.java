package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
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
import jakarta.persistence.OneToMany;
import jakarta.persistence.CascadeType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Enumerated;
import jakarta.persistence.EnumType;
import jakarta.persistence.Embedded;
import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.UniqueConstraint;

import java.time.LocalDateTime;
import java.time.LocalDate;
import java.math.BigDecimal;
import java.util.List;

import jakarta.validation.constraints.*;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovement;
import com.sajjadkademm.retail.users.User;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonBackReference;

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
                @Index(name = "idx_inventory_items_barcode", columnList = "barcode"),
                @Index(name = "idx_inventory_items_inventory_id", columnList = "inventory_id"),
                @Index(name = "idx_inventory_items_barcode_inventory", columnList = "barcode,inventory_id"),
                @Index(name = "idx_inventory_items_product_code", columnList = "product_code"),
                @Index(name = "idx_inventory_items_category", columnList = "category"),
                @Index(name = "idx_inventory_items_brand", columnList = "brand"),
                @Index(name = "idx_inventory_items_supplier_name", columnList = "supplier_name"),
                @Index(name = "idx_inventory_items_is_active", columnList = "is_active"),
                @Index(name = "idx_inventory_items_expiry_date", columnList = "expiry_date"),
                @Index(name = "idx_inventory_items_low_stock", columnList = "current_stock,minimum_stock")
}, uniqueConstraints = {
                @UniqueConstraint(name = "uk_inventory_items_barcode_inventory", columnNames = { "barcode",
                                "inventory_id" }),
                @UniqueConstraint(name = "uk_inventory_items_product_code_inventory", columnNames = { "product_code",
                                "inventory_id" })
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

        // Internal product code for business operations
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

        // Stock Management
        // Current available stock quantity
        @Column(name = "current_stock", nullable = false)
        @NotNull
        @Min(value = 0, message = "Current stock cannot be negative")
        private Integer currentStock;

        // Minimum stock level before reordering alert
        @Column(name = "minimum_stock")
        @Min(value = 0, message = "Minimum stock cannot be negative")
        private Integer minimumStock;

        // Maximum stock capacity for storage optimization
        @Column(name = "maximum_stock")
        @Min(value = 0, message = "Maximum stock cannot be negative")
        private Integer maximumStock;

        // Pricing Information
        // Cost price paid to supplier (amount + currency)
        @Embedded
        @AttributeOverrides({
                        @AttributeOverride(name = "amount", column = @Column(name = "cost_price", precision = 10, scale = 2, nullable = true)),
                        @AttributeOverride(name = "currency", column = @Column(name = "cost_currency", length = 3, nullable = true))
        })
        private Money costPrice;

        // Regular selling price to customers (amount + currency)
        @Embedded
        @AttributeOverrides({
                        @AttributeOverride(name = "amount", column = @Column(name = "selling_price", precision = 10, scale = 2, nullable = false)),
                        @AttributeOverride(name = "currency", column = @Column(name = "selling_currency", length = 3, nullable = false))
        })
        @NotNull
        private Money sellingPrice;

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
        @Column(name = "is_perishable")
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

        // Inventory Association
        // Foreign key reference to the inventory this item belongs to
        @Column(name = "inventory_id", nullable = false)
        @NotNull(message = "Inventory ID is required")
        private String inventoryId;

        // Inventory Movements (One-to-Many relationship)
        @OneToMany(mappedBy = "inventoryItem", cascade = CascadeType.ALL, orphanRemoval = true)
        @JsonIgnoreProperties("inventoryItem")
        private List<InventoryMovement> inventoryMovements;

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
        // Many-to-One relationship with User entity (tracks who created this item)
        @ManyToOne(optional = false, fetch = FetchType.EAGER)
        @JsonIgnoreProperties({ "password", "phone", "lastLoginAt" })
        @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
        private User createdBy;
}
