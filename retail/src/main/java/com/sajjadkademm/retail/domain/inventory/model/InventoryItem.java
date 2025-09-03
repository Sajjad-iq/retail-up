package com.sajjadkademm.retail.domain.inventory.model;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import com.sajjadkademm.retail.shared.enums.Money;
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

import com.sajjadkademm.retail.shared.enums.Unit;
// REMOVED: InventoryMovement import - now using GlobalAuditService for tracking
import com.sajjadkademm.retail.domain.auth.model.User;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

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
        @NotBlank(message = "{name.required}")
        @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH, message = "{name.required}")
        private String name;

        // Detailed product description for customer information and internal reference
        @Column(name = "description", length = ValidationConstants.MAX_DESCRIPTION_LENGTH)
        @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH, message = "{inventory.description.invalid}")
        private String description;

        // Internal product code for business operations
        @Column(name = "product_code", length = ValidationConstants.MAX_PRODUCT_CODE_LENGTH)
        @Size(max = ValidationConstants.MAX_PRODUCT_CODE_LENGTH, message = "{inventoryItem.product.code.too.long}")
        private String productCode;

        // Product Identification
        // Barcode for POS scanning and quick identification
        @Column(name = "barcode", length = ValidationConstants.MAX_BARCODE_LENGTH)
        @Size(max = ValidationConstants.MAX_BARCODE_LENGTH, message = "{inventoryItem.barcode.too.long}")
        private String barcode;

        // Product Classification
        // Category for organizing products (e.g., Electronics, Clothing, Food)
        @Column(name = "category", length = ValidationConstants.MAX_CATEGORY_LENGTH)
        @Size(max = ValidationConstants.MAX_CATEGORY_LENGTH)
        private String category;

        // Brand name for filtering and customer preference
        @Column(name = "brand", length = ValidationConstants.MAX_BRAND_LENGTH)
        @Size(max = ValidationConstants.MAX_BRAND_LENGTH)
        private String brand;

        // Measurement unit for the product (pieces, kg, liters, etc.)
        @Column(name = "unit", nullable = false)
        @Enumerated(EnumType.STRING)
        @NotNull(message = "{unit.required}")
        private Unit unit;

        // Physical Attributes
        // Product weight in grams for shipping calculations and specifications
        @Column(name = "weight", precision = 8, scale = 2)
        @DecimalMin(value = "0.0", message = "{inventoryItem.weight.negative}")
        private BigDecimal weight;

        // Product dimensions in format "Length x Width x Height" (in cm)
        @Column(name = "dimensions", length = ValidationConstants.MAX_DIMENSIONS_LENGTH)
        @Size(max = ValidationConstants.MAX_DIMENSIONS_LENGTH)
        private String dimensions;

        // Product Variants
        // Color variant for products available in multiple colors
        @Column(name = "color", length = ValidationConstants.MAX_COLOR_LENGTH)
        @Size(max = ValidationConstants.MAX_COLOR_LENGTH)
        private String color;

        // Size variant for clothing, shoes, etc. (S, M, L, XL, or numeric sizes)
        @Column(name = "size", length = ValidationConstants.MAX_SIZE_LENGTH)
        @Size(max = ValidationConstants.MAX_SIZE_LENGTH)
        private String size;

        // Stock Management
        // Current available stock quantity
        @Column(name = "current_stock", nullable = false)
        @NotNull(message = "{inventoryItem.current.stock.required}")
        @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
        private Integer currentStock;

        // Minimum stock level before reordering alert
        @Column(name = "minimum_stock")
        @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
        private Integer minimumStock;

        // Maximum stock capacity for storage optimization
        @Column(name = "maximum_stock")
        @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
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
        @DecimalMin(value = "0.0", message = "{inventoryItem.discount.price.negative}")
        private BigDecimal discountPrice;

        // Start date for discount pricing
        @Column(name = "discount_start_date")
        private LocalDateTime discountStartDate;

        // End date for discount pricing
        @Column(name = "discount_end_date")
        private LocalDateTime discountEndDate;

        // Supplier name for quick reference without joining tables
        @Column(name = "supplier_name", length = ValidationConstants.MAX_SUPPLIER_LENGTH)
        @Size(max = ValidationConstants.MAX_SUPPLIER_LENGTH)
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
        @NotNull(message = \"{invalid.inventory.data}\")
        @Builder.Default
        private Boolean isActive = true;

        // Inventory Association
        // Foreign key reference to the inventory this item belongs to
        @Column(name = "inventory_id", nullable = false)
        @NotNull(message = "{inventory.id.required}")
        private String inventoryId;

        // REMOVED: Inventory Movements relationship - now using GlobalAuditService
        // All inventory tracking is handled by global_audit_logs table
        // Use GlobalAuditRepository to query inventory movement history

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
