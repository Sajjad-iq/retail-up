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

import java.time.LocalDateTime;
import java.math.BigDecimal;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.DecimalMin;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.sajjadkademm.retail.inventory.Inventory;
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
        @Index(name = "idx_inventory_items_barcode_inventory", columnList = "barcode,inventory_id")
})
public class InventoryItem {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "name", nullable = false)
    @NotBlank
    @Size(min = 2, max = 200)
    private String name;

    @Column(name = "description", length = 1000)
    @Size(max = 1000)
    private String description;

    @Column(name = "sku", nullable = false)
    @NotBlank
    @Size(min = 3, max = 50)
    private String sku;

    @Column(name = "barcode", length = 100)
    @Size(max = 100)
    private String barcode;

    @Column(name = "category", length = 100)
    @Size(max = 100)
    private String category;

    @Column(name = "brand", length = 100)
    @Size(max = 100)
    private String brand;

    @Column(name = "unit", length = 20, nullable = false)
    @NotBlank
    @Size(max = 20)
    private String unit;

    @Column(name = "current_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "Current stock cannot be negative")
    private Integer currentStock;

    @Column(name = "minimum_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "Minimum stock cannot be negative")
    private Integer minimumStock;

    @Column(name = "maximum_stock")
    @Min(value = 0, message = "Maximum stock cannot be negative")
    private Integer maximumStock;

    @Column(name = "cost_price", precision = 10, scale = 2)
    @DecimalMin(value = "0.0", message = "Cost price cannot be negative")
    private BigDecimal costPrice;

    @Column(name = "selling_price", precision = 10, scale = 2, nullable = false)
    @NotNull
    @DecimalMin(value = "0.0", message = "Selling price cannot be negative")
    private BigDecimal sellingPrice;

    @Column(name = "is_active", nullable = false)
    @NotNull
    private Boolean isActive;

    @Column(name = "inventory_id", nullable = false)
    @NotNull(message = "Inventory ID is required")
    private String inventoryId;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "inventory_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_items_inventory"))
    private Inventory inventory;

    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}