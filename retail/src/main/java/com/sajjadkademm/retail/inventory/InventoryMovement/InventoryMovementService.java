package com.sajjadkademm.retail.inventory.InventoryMovement;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemService;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.CreateMovementRequest;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
public class InventoryMovementService {
    private final InventoryMovementRepository movementRepository;
    private final InventoryItemService inventoryItemService;
    private final UserRepository userRepository;

    public InventoryMovementService(InventoryMovementRepository movementRepository,
            InventoryItemService inventoryItemService,
            UserRepository userRepository) {
        this.movementRepository = movementRepository;
        this.inventoryItemService = inventoryItemService;
        this.userRepository = userRepository;
    }

    /**
     * Record a new inventory movement
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordMovement(CreateMovementRequest request) {
        try {
            // Validate inventory item exists
            InventoryItem item = inventoryItemService.getInventoryItemById(request.getInventoryItemId());

            // Validate user exists
            User user = userRepository.findById(request.getUserId())
                    .orElseThrow(() -> new NotFoundException("User not found with ID: " + request.getUserId()));

            // Calculate new stock level
            int previousStock = item.getCurrentStock();
            int newStock = calculateNewStock(previousStock, request.getQuantity(), request.getMovementType());

            // Validate stock doesn't go negative
            if (newStock < 0) {
                throw new BadRequestException(
                        "Insufficient stock. Current stock: " + previousStock + ", requested: "
                                + Math.abs(request.getQuantity()));
            }

            // Create movement record (entity no longer stores previous/new stock)
            InventoryMovement movement = InventoryMovement.builder()
                    .movementType(request.getMovementType())
                    .quantity(request.getQuantity())
                    .reason(request.getReason())
                    .referenceType(request.getReferenceType())
                    .referenceId(request.getReferenceId())
                    .inventoryItem(item)
                    .createdBy(user)
                    .build();

            // Save movement record
            InventoryMovement savedMovement = movementRepository.save(movement);

            // Update inventory item stock
            updateInventoryItemStock(item, newStock);

            return savedMovement;

        } catch (NotFoundException | BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to record inventory movement: " + e.getMessage(), e);
        }
    }

    /**
     * Get movement by ID
     */
    public InventoryMovement getMovementById(String id) {
        return movementRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory movement not found with ID: " + id));
    }

    /**
     * Get movements for an inventory item
     */
    public List<InventoryMovement> getMovementsByItem(String inventoryItemId) {
        return movementRepository.findByInventoryItemIdOrderByCreatedAtDesc(inventoryItemId);
    }

    /**
     * Get movements for an inventory item with pagination
     */
    public Page<InventoryMovement> getMovementsByItem(String inventoryItemId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        return movementRepository.findByInventoryItemIdOrderByCreatedAtDesc(inventoryItemId, pageable);
    }

    /**
     * Get movements for an inventory
     */
    public List<InventoryMovement> getMovementsByInventory(String inventoryId) {
        return movementRepository.findByInventoryIdOrderByCreatedAtDesc(inventoryId);
    }

    /**
     * Get movements for an inventory with pagination
     */
    public Page<InventoryMovement> getMovementsByInventory(String inventoryId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        return movementRepository.findByInventoryIdOrderByCreatedAtDesc(inventoryId, pageable);
    }

    /**
     * Get movements by type
     */
    public List<InventoryMovement> getMovementsByType(MovementType movementType) {
        return movementRepository.findByMovementTypeOrderByCreatedAtDesc(movementType);
    }

    /**
     * Get movements by user
     */
    public List<InventoryMovement> getMovementsByUser(String userId) {
        return movementRepository.findByCreatedByIdOrderByCreatedAtDesc(userId);
    }

    /**
     * Get movements by date range
     */
    public List<InventoryMovement> getMovementsByDateRange(LocalDateTime startDate, LocalDateTime endDate) {
        return movementRepository.findByDateRange(startDate, endDate);
    }

    /**
     * Get movements by reference
     */
    public List<InventoryMovement> getMovementsByReference(String referenceType, String referenceId) {
        return movementRepository.findByReferenceTypeAndReferenceIdOrderByCreatedAtDesc(referenceType, referenceId);
    }

    /**
     * Calculate new stock based on movement type and quantity
     */
    private int calculateNewStock(int currentStock, int quantity, MovementType movementType) {
        switch (movementType) {
            case STOCK_IN:
            case PURCHASE:
            case RETURN:
            case ADJUSTMENT_IN:
            case TRANSFER_IN:
                return currentStock + Math.abs(quantity);
            case STOCK_OUT:
            case SALE:
            case DAMAGE:
            case THEFT:
            case EXPIRED:
            case ADJUSTMENT_OUT:
            case TRANSFER_OUT:
                return currentStock - Math.abs(quantity);

            case MANUAL_ADJUSTMENT:
                return currentStock + quantity; // Can be positive or negative

            default:
                throw new BadRequestException("Unknown movement type: " + movementType);
        }
    }

    /**
     * Update inventory item stock
     */
    private void updateInventoryItemStock(InventoryItem item, int newStock) {
        // Use the inventory item service to update stock
        inventoryItemService.updateStock(item.getId(), newStock);
    }
}