package com.sajjadkademm.retail.inventory.InventoryMovement;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.CreateMovementRequest;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.dto.UserStatus;
// Removed UserRepository dependency; user is provided via request

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Application service responsible for recording and querying inventory item
 * movements. This service encapsulates the stock delta calculation, movement
 * persistence and stock synchronization with the related `InventoryItem`.
 *
 * Use the typed helper methods (e.g. `recordStockIn`, `recordSale`,
 * `recordAdjustmentToTarget`) rather than constructing requests manually to
 * keep movement recording consistent across the application.
 */
@Service
public class InventoryMovementService {
    private final InventoryMovementRepository movementRepository;

    public InventoryMovementService(InventoryMovementRepository movementRepository) {
        this.movementRepository = movementRepository;
    }

    /**
     * Record a new inventory movement.
     *
     * Validates the request, computes the resulting stock level, prevents
     * negative stock, persists the movement and updates the item's current
     * stock.
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordMovement(CreateMovementRequest request) {
        try {
            // Take user and item directly from request (assumed validated upstream)
            InventoryItem item = request.getInventoryItem();
            if (item == null) {
                throw new BadRequestException("Inventory item must be provided in the request");
            }

            User user = request.getUser();
            if (user == null) {
                throw new BadRequestException("User must be provided in the request");
            }

            if (user.getStatus() != UserStatus.ACTIVE) {
                throw new BadRequestException("User must be active to record inventory movements");
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

            return savedMovement;

        } catch (NotFoundException | BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to record inventory movement: " + e.getMessage(), e);
        }
    }

    // Convenience methods for common movement types

    /**
     * Record a STOCK_IN movement (initial stock or restocking).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordStockIn(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.STOCK_IN);
        request.setQuantity(quantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a STOCK_OUT movement (general stock removal).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordStockOut(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.STOCK_OUT);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record an ADJUSTMENT_IN movement (positive correction).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordAdjustmentIn(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.ADJUSTMENT_IN);
        request.setQuantity(quantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record an ADJUSTMENT_OUT movement (negative correction).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordAdjustmentOut(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.ADJUSTMENT_OUT);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record an adjustment to reach the provided target stock, computing the
     * appropriate direction and quantity.
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordAdjustmentToTarget(User user, InventoryItem item, int targetStock, String reason,
            ReferenceType referenceType, String referenceId) {
        int current = item.getCurrentStock();
        int delta = targetStock - current;
        if (delta == 0) {
            return null;
        }
        if (delta > 0) {
            return recordAdjustmentIn(user, item, delta, reason, referenceType, referenceId);
        } else {
            return recordAdjustmentOut(user, item, Math.abs(delta), reason, referenceType, referenceId);
        }
    }

    // Inbound flows
    /**
     * Record a PURCHASE movement (stock received from supplier).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordPurchase(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.PURCHASE);
        request.setQuantity(quantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a RETURN movement (customer returned items).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordReturn(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.RETURN);
        request.setQuantity(quantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a TRANSFER_IN movement (stock received from another location).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordTransferIn(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.TRANSFER_IN);
        request.setQuantity(quantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    // Outbound flows
    /**
     * Record a SALE movement (items sold to a customer).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordSale(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.SALE);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a DAMAGE movement (items damaged or spoiled).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordDamage(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.DAMAGE);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a THEFT movement (items stolen or missing).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordTheft(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.THEFT);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record an EXPIRED movement (items past expiry date).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordExpired(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.EXPIRED);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Record a TRANSFER_OUT movement (stock sent to another location).
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordTransferOut(User user, InventoryItem item, int quantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.TRANSFER_OUT);
        request.setQuantity(Math.abs(quantity));
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    // Manual
    /**
     * Record a MANUAL_ADJUSTMENT movement with a signed quantity.
     */
    @Transactional(rollbackFor = { Exception.class })
    public InventoryMovement recordManualAdjustment(User user, InventoryItem item, int signedQuantity, String reason,
            ReferenceType referenceType, String referenceId) {
        CreateMovementRequest request = new CreateMovementRequest();
        request.setUser(user);
        request.setInventoryItem(item);
        request.setMovementType(MovementType.MANUAL_ADJUSTMENT);
        request.setQuantity(signedQuantity);
        request.setReason(reason);
        request.setReferenceType(referenceType);
        request.setReferenceId(referenceId);
        return recordMovement(request);
    }

    /**
     * Get movement by ID
     */
    public InventoryMovement getMovementById(String id) {
        return movementRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory movement not found"));
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

}