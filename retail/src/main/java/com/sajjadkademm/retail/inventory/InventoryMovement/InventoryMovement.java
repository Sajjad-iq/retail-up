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

import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.NotNull;

import org.hibernate.annotations.CreationTimestamp;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
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

    // Quantity moved
    @Column(name = "quantity", nullable = false)
    @NotNull
    private Integer quantity;

    // Optional reason or notes for the movement
    @Column(name = "reason", length = 500)
    @Size(max = 500)
    private String reason;

    // Reference Information
    @Column(name = "reference_type", length = 50)
    @Enumerated(EnumType.STRING)
    private ReferenceType referenceType;

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
    @JsonIgnoreProperties({ "password", "phone", "lastLoginAt" })
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}