package com.sajjadkademm.retail.inventory.InventoryMovement;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;

import java.time.LocalDateTime;
import java.util.List;

public interface InventoryMovementRepository
        extends JpaRepository<InventoryMovement, String>, PagingAndSortingRepository<InventoryMovement, String> {

    // Find movements by inventory item
    List<InventoryMovement> findByInventoryItemIdOrderByCreatedAtDesc(String inventoryItemId);

    // Find movements by inventory item with pagination
    Page<InventoryMovement> findByInventoryItemIdOrderByCreatedAtDesc(String inventoryItemId, Pageable pageable);

    // Find movements by movement type
    List<InventoryMovement> findByMovementTypeOrderByCreatedAtDesc(MovementType movementType);

    // Find movements by movement type with pagination
    Page<InventoryMovement> findByMovementTypeOrderByCreatedAtDesc(MovementType movementType, Pageable pageable);

    // Find movements by user
    List<InventoryMovement> findByCreatedByIdOrderByCreatedAtDesc(String userId);

    // Find movements by user with pagination
    Page<InventoryMovement> findByCreatedByIdOrderByCreatedAtDesc(String userId, Pageable pageable);

    // Find movements by date range
    @Query("SELECT m FROM InventoryMovement m WHERE m.createdAt BETWEEN :startDate AND :endDate ORDER BY m.createdAt DESC")
    List<InventoryMovement> findByDateRange(@Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate);

    // Find movements by date range with pagination
    @Query("SELECT m FROM InventoryMovement m WHERE m.createdAt BETWEEN :startDate AND :endDate ORDER BY m.createdAt DESC")
    Page<InventoryMovement> findByDateRange(@Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate, Pageable pageable);

    // Find movements by inventory item and date range
    @Query("SELECT m FROM InventoryMovement m WHERE m.inventoryItem.id = :inventoryItemId AND m.createdAt BETWEEN :startDate AND :endDate ORDER BY m.createdAt DESC")
    List<InventoryMovement> findByInventoryItemAndDateRange(@Param("inventoryItemId") String inventoryItemId,
            @Param("startDate") LocalDateTime startDate, @Param("endDate") LocalDateTime endDate);

    // Find movements by reference
    List<InventoryMovement> findByReferenceTypeAndReferenceIdOrderByCreatedAtDesc(String referenceType,
            String referenceId);

    // Find movements for items in a specific inventory
    @Query("SELECT m FROM InventoryMovement m WHERE m.inventoryItem.inventoryId = :inventoryId ORDER BY m.createdAt DESC")
    List<InventoryMovement> findByInventoryIdOrderByCreatedAtDesc(@Param("inventoryId") String inventoryId);

    // Find movements for items in a specific inventory with pagination
    @Query("SELECT m FROM InventoryMovement m WHERE m.inventoryItem.inventoryId = :inventoryId ORDER BY m.createdAt DESC")
    Page<InventoryMovement> findByInventoryIdOrderByCreatedAtDesc(@Param("inventoryId") String inventoryId,
            Pageable pageable);
}