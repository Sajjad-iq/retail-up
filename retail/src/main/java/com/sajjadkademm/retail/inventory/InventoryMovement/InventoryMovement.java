package com.sajjadkademm.retail.inventory.InventoryMovement;

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

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Min;

import org.hibernate.annotations.CreationTimestamp;

import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;
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
@Table(name = "inventory_movements", indexes = {
        @Index(name = "idx_inventory_movements_item_id", columnList = "inventory_item_id"),
        @Index(name = "idx_inventory_movements_type", columnList = "movement_type"),
        @Index(name = "idx_inventory_movements_created_at", columnList = "created_at"),
        @Index(name = "idx_inventory_movements_created_by", columnList = "created_by"),
        @Index(name = "idx_inventory_movements_reference", columnList = "reference_type,reference_id")
})
public class InventoryMovement {
    // Primary Key - Unique identifier for each movement record
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    // Movement Details
    // Type of movement (IN, OUT, ADJUSTMENT, TRANSFER, etc.)
    @Column(name = "movement_type", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull
    private MovementType movementType;

    // Quantity moved (positive for IN, negative for OUT)
    @Column(name = "quantity", nullable = false)
    @NotNull
    private Integer quantity;

    // Stock level before this movement
    @Column(name = "previous_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "Previous stock cannot be negative")
    private Integer previousStock;

    // Stock level after this movement
    @Column(name = "new_stock", nullable = false)
    @NotNull
    @Min(value = 0, message = "New stock cannot be negative")
    private Integer newStock;

    // Optional reason or notes for the movement
    @Column(name = "reason", length = 500)
    @Size(max = 500)
    private String reason;

    // Reference Information
    // Type of reference (SALE, PURCHASE, ADJUSTMENT, TRANSFER, etc.)
    @Column(name = "reference_type", length = 50)
    @Size(max = 50)
    private String referenceType;

    // ID of the referenced entity (sale ID, purchase ID, etc.)
    @Column(name = "reference_id", length = 100)
    @Size(max = 100)
    private String referenceId;

    // Audit Trail
    // Timestamp when the movement was recorded (auto-generated)
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    // Entity Relationships
    // Many-to-One relationship with InventoryItem entity
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "inventory_item_id", nullable = false, foreignKey = @ForeignKey(name = "fk_inventory_movements_item"))
    private InventoryItem inventoryItem;

    // Many-to-One relationship with User entity (tracks who made the movement)
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}