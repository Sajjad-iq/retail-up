package com.sajjadkademm.retail.inventory.InventoryMovement;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemService;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.CreateMovementRequest;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.MovementType;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SpringBootTest
@ActiveProfiles("test")
class InventoryMovementServiceTest {

    @Autowired
    private InventoryMovementService inventoryMovementService;

    @MockitoBean
    private InventoryMovementRepository movementRepository;

    @MockitoBean
    private InventoryItemService inventoryItemService;

    private User testUser;
    private InventoryItem testItem;

    @BeforeEach
    void setUp() {
        testUser = User.builder()
                .id("user-1")
                .name("Tester")
                .email("t@example.com")
                .phone("000")
                .password("x")
                .status(UserStatus.ACTIVE)
                .accountType(AccountType.USER)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        testItem = InventoryItem.builder()
                .id("item-1")
                .name("Sample")
                .sku("SKU-1")
                .currentStock(5)
                .createdBy(testUser)
                .build();

        when(movementRepository.save(any(InventoryMovement.class)))
                .thenAnswer(inv -> inv.getArgument(0));
    }

    @Test
    @DisplayName("recordStockIn should create movement and increase stock")
    void recordStockIn_ShouldIncreaseStock() {
        inventoryMovementService.recordStockIn(testUser, testItem, 3, "Restock", ReferenceType.PURCHASE, "ref-1");
        verify(movementRepository)
                .save(argThat(m -> m.getMovementType() == MovementType.STOCK_IN && m.getQuantity() == 3));
        verify(inventoryItemService).updateStock("item-1", 8);
    }

    @Test
    @DisplayName("recordStockOut should create movement and decrease stock")
    void recordStockOut_ShouldDecreaseStock() {
        inventoryMovementService.recordStockOut(testUser, testItem, 2, "Usage", ReferenceType.OTHER, "ref-2");
        verify(movementRepository)
                .save(argThat(m -> m.getMovementType() == MovementType.STOCK_OUT && m.getQuantity() == 2));
        verify(inventoryItemService).updateStock("item-1", 3);
    }

    @Test
    @DisplayName("recordStockOut with insufficient stock should fail")
    void recordStockOut_InsufficientStock_ShouldFail() {
        assertThrows(BadRequestException.class, () -> inventoryMovementService.recordStockOut(testUser, testItem, 10,
                "Excess", ReferenceType.OTHER, "ref-3"));
        verify(inventoryItemService, never()).updateStock(anyString(), anyInt());
    }

    @Test
    @DisplayName("recordStockOut with negative qty should still decrease by abs(qty)")
    void recordStockOut_NegativeQty_UsesAbs() {
        inventoryMovementService.recordStockOut(testUser, testItem, -2, "Correction", ReferenceType.OTHER, "ref-2n");
        verify(movementRepository)
                .save(argThat(m -> m.getMovementType() == MovementType.STOCK_OUT && m.getQuantity() == 2));
        verify(inventoryItemService).updateStock("item-1", 3);
    }

    @Test
    @DisplayName("recordAdjustmentToTarget should compute delta and update to target")
    void recordAdjustmentToTarget_ShouldReachTarget() {
        inventoryMovementService.recordAdjustmentToTarget(testUser, testItem, 12, "Sync", ReferenceType.ADJUSTMENT,
                "ref-4");
        // delta +7 -> ADJUSTMENT_IN with 7
        verify(movementRepository)
                .save(argThat(m -> m.getMovementType() == MovementType.ADJUSTMENT_IN && m.getQuantity() == 7));
        verify(inventoryItemService).updateStock("item-1", 12);
    }

    @Test
    @DisplayName("recordAdjustmentToTarget with no delta should return null and do nothing")
    void recordAdjustmentToTarget_NoDelta_ShouldNoop() {
        InventoryItem item = InventoryItem.builder()
                .id("item-1")
                .name("Sample")
                .sku("SKU-1")
                .currentStock(10)
                .createdBy(testUser)
                .build();
        InventoryMovement res = inventoryMovementService.recordAdjustmentToTarget(testUser, item, 10, "Sync",
                ReferenceType.ADJUSTMENT, "ref-5");
        assertNull(res);
        verify(movementRepository, never()).save(any());
        verify(inventoryItemService, never()).updateStock(anyString(), anyInt());
    }

    @Nested
    @DisplayName("Direct recordMovement validations")
    class DirectRecordMovementValidations {
        @Test
        @DisplayName("recordMovement without item should fail")
        void recordMovement_NoItem_ShouldFail() {
            CreateMovementRequest req = new CreateMovementRequest();
            req.setUser(testUser);
            req.setMovementType(MovementType.STOCK_IN);
            req.setQuantity(1);
            assertThrows(BadRequestException.class, () -> inventoryMovementService.recordMovement(req));
        }

        @Test
        @DisplayName("recordMovement without user should fail")
        void recordMovement_NoUser_ShouldFail() {
            CreateMovementRequest req = new CreateMovementRequest();
            req.setInventoryItem(testItem);
            req.setMovementType(MovementType.STOCK_IN);
            req.setQuantity(1);
            assertThrows(BadRequestException.class, () -> inventoryMovementService.recordMovement(req));
        }
    }

    @Nested
    @DisplayName("Quantity normalization & edge cases")
    class Normalization {
        @Test
        @DisplayName("recordStockIn with negative qty should still increase by abs(qty)")
        void recordStockIn_NegativeQty_UsesAbsForStock() {
            inventoryMovementService.recordStockIn(testUser, testItem, -4, "Restock", ReferenceType.PURCHASE, "ref-n");
            // Movement saved with raw quantity (-4)
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.STOCK_IN && m.getQuantity() == -4));
            // Stock increased by abs(4)
            verify(inventoryItemService).updateStock("item-1", 9);
        }

        @Test
        @DisplayName("recordAdjustmentOut insufficient should fail")
        void recordAdjustmentOut_Insufficient_Fails() {
            assertThrows(BadRequestException.class, () -> inventoryMovementService.recordAdjustmentOut(testUser,
                    testItem, 99, "Adj out", ReferenceType.ADJUSTMENT, "a-out"));
            verify(inventoryItemService, never()).updateStock(anyString(), anyInt());
        }
    }

    @Nested
    @DisplayName("Query methods")
    class Queries {
        private InventoryMovement sample(String id, MovementType type, int qty) {
            return InventoryMovement.builder()
                    .id(id)
                    .movementType(type)
                    .quantity(qty)
                    .referenceType(ReferenceType.OTHER)
                    .referenceId("ref")
                    .inventoryItem(testItem)
                    .createdBy(testUser)
                    .build();
        }

        @Test
        @DisplayName("getMovementById returns the movement")
        void getMovementById_Returns() {
            InventoryMovement mov = sample("m-1", MovementType.SALE, 1);
            when(movementRepository.findById("m-1")).thenReturn(Optional.of(mov));
            InventoryMovement found = inventoryMovementService.getMovementById("m-1");
            assertEquals("m-1", found.getId());
            assertEquals(MovementType.SALE, found.getMovementType());
        }

        @Test
        @DisplayName("getMovementsByItem returns list")
        void getMovementsByItem_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-1", MovementType.STOCK_IN, 2));
            when(movementRepository.findByInventoryItemIdOrderByCreatedAtDesc("item-1")).thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService.getMovementsByItem("item-1");
            assertEquals(1, res.size());
        }

        @Test
        @DisplayName("getMovementsByItem (paged) returns page")
        void getMovementsByItem_Paged_ReturnsPage() {
            List<InventoryMovement> list = Arrays.asList(sample("m-1", MovementType.STOCK_IN, 2));
            when(movementRepository.findByInventoryItemIdOrderByCreatedAtDesc(eq("item-1"), any()))
                    .thenReturn(new PageImpl<>(list, PageRequest.of(0, 10), 1));
            Page<InventoryMovement> page = inventoryMovementService.getMovementsByItem("item-1", 0, 10);
            assertEquals(1, page.getTotalElements());
            assertEquals(1, page.getContent().size());
        }

        @Test
        @DisplayName("getMovementsByInventory returns list")
        void getMovementsByInventory_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-2", MovementType.SALE, 1));
            when(movementRepository.findByInventoryIdOrderByCreatedAtDesc("inv-1")).thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService.getMovementsByInventory("inv-1");
            assertEquals(1, res.size());
        }

        @Test
        @DisplayName("getMovementsByInventory (paged) returns page")
        void getMovementsByInventory_Paged_ReturnsPage() {
            List<InventoryMovement> list = Arrays.asList(sample("m-2", MovementType.SALE, 1));
            when(movementRepository.findByInventoryIdOrderByCreatedAtDesc(eq("inv-1"), any()))
                    .thenReturn(new PageImpl<>(list, PageRequest.of(0, 20), 1));
            Page<InventoryMovement> page = inventoryMovementService.getMovementsByInventory("inv-1", 0, 20);
            assertEquals(1, page.getContent().size());
        }

        @Test
        @DisplayName("getMovementsByType returns list")
        void getMovementsByType_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-3", MovementType.PURCHASE, 5));
            when(movementRepository.findByMovementTypeOrderByCreatedAtDesc(MovementType.PURCHASE)).thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService.getMovementsByType(MovementType.PURCHASE);
            assertEquals(1, res.size());
            assertEquals(MovementType.PURCHASE, res.get(0).getMovementType());
        }

        @Test
        @DisplayName("getMovementsByUser returns list")
        void getMovementsByUser_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-4", MovementType.RETURN, 2));
            when(movementRepository.findByCreatedByIdOrderByCreatedAtDesc("user-1")).thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService.getMovementsByUser("user-1");
            assertEquals(1, res.size());
            assertEquals("m-4", res.get(0).getId());
        }

        @Test
        @DisplayName("getMovementsByDateRange returns list")
        void getMovementsByDateRange_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-5", MovementType.TRANSFER_IN, 3));
            when(movementRepository.findByDateRange(any(), any())).thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService
                    .getMovementsByDateRange(LocalDateTime.now().minusDays(1), LocalDateTime.now());
            assertEquals(1, res.size());
        }

        @Test
        @DisplayName("getMovementsByReference returns list")
        void getMovementsByReference_ReturnsList() {
            List<InventoryMovement> list = Arrays.asList(sample("m-6", MovementType.TRANSFER_OUT, 1));
            when(movementRepository.findByReferenceTypeAndReferenceIdOrderByCreatedAtDesc("SALE", "ref-9"))
                    .thenReturn(list);
            List<InventoryMovement> res = inventoryMovementService.getMovementsByReference("SALE", "ref-9");
            assertEquals(1, res.size());
            assertEquals("m-6", res.get(0).getId());
        }
    }

    @Nested
    @DisplayName("Inbound helpers")
    class InboundHelpers {
        @Test
        @DisplayName("recordPurchase increases stock")
        void recordPurchase_IncreasesStock() {
            inventoryMovementService.recordPurchase(testUser, testItem, 4, "PO", ReferenceType.PURCHASE, "po-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.PURCHASE && m.getQuantity() == 4));
            verify(inventoryItemService).updateStock("item-1", 9);
        }

        @Test
        @DisplayName("recordReturn increases stock")
        void recordReturn_IncreasesStock() {
            inventoryMovementService.recordReturn(testUser, testItem, 2, "Customer return", ReferenceType.RETURN,
                    "r-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.RETURN && m.getQuantity() == 2));
            verify(inventoryItemService).updateStock("item-1", 7);
        }

        @Test
        @DisplayName("recordTransferIn increases stock")
        void recordTransferIn_IncreasesStock() {
            inventoryMovementService.recordTransferIn(testUser, testItem, 6, "From WH2", ReferenceType.TRANSFER,
                    "t-in-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.TRANSFER_IN && m.getQuantity() == 6));
            verify(inventoryItemService).updateStock("item-1", 11);
        }
    }

    @Nested
    @DisplayName("Outbound helpers")
    class OutboundHelpers {
        @Test
        @DisplayName("recordSale decreases stock")
        void recordSale_DecreasesStock() {
            inventoryMovementService.recordSale(testUser, testItem, 3, "POS sale", ReferenceType.SALE, "s-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.SALE && m.getQuantity() == 3));
            verify(inventoryItemService).updateStock("item-1", 2);
        }

        @Test
        @DisplayName("recordSale insufficient stock should fail")
        void recordSale_InsufficientStock_Fails() {
            assertThrows(BadRequestException.class, () -> inventoryMovementService.recordSale(testUser, testItem, 9,
                    "POS sale", ReferenceType.SALE, "s-2"));
            verify(inventoryItemService, never()).updateStock(anyString(), anyInt());
        }

        @Test
        @DisplayName("recordDamage decreases stock")
        void recordDamage_DecreasesStock() {
            inventoryMovementService.recordDamage(testUser, testItem, 2, "Broken", ReferenceType.DAMAGE, "d-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.DAMAGE && m.getQuantity() == 2));
            verify(inventoryItemService).updateStock("item-1", 3);
        }

        @Test
        @DisplayName("recordTheft decreases stock")
        void recordTheft_DecreasesStock() {
            inventoryMovementService.recordTheft(testUser, testItem, 1, "Missing", ReferenceType.THEFT, "th-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.THEFT && m.getQuantity() == 1));
            verify(inventoryItemService).updateStock("item-1", 4);
        }

        @Test
        @DisplayName("recordExpired decreases stock")
        void recordExpired_DecreasesStock() {
            inventoryMovementService.recordExpired(testUser, testItem, 1, "Expired", ReferenceType.OTHER, "ex-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.EXPIRED && m.getQuantity() == 1));
            verify(inventoryItemService).updateStock("item-1", 4);
        }

        @Test
        @DisplayName("recordTransferOut decreases stock")
        void recordTransferOut_DecreasesStock() {
            inventoryMovementService.recordTransferOut(testUser, testItem, 2, "To WH2", ReferenceType.TRANSFER,
                    "t-out-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.TRANSFER_OUT && m.getQuantity() == 2));
            verify(inventoryItemService).updateStock("item-1", 3);
        }
    }

    @Nested
    @DisplayName("Manual adjustment")
    class ManualAdjustment {
        @Test
        @DisplayName("Positive manual adjustment increases stock")
        void manualPositive_IncreasesStock() {
            inventoryMovementService.recordManualAdjustment(testUser, testItem, 4, "Count fix",
                    ReferenceType.ADJUSTMENT, "m-1");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.MANUAL_ADJUSTMENT && m.getQuantity() == 4));
            verify(inventoryItemService).updateStock("item-1", 9);
        }

        @Test
        @DisplayName("Negative manual adjustment decreases stock")
        void manualNegative_DecreasesStock() {
            inventoryMovementService.recordManualAdjustment(testUser, testItem, -3, "Count fix",
                    ReferenceType.ADJUSTMENT, "m-2");
            verify(movementRepository)
                    .save(argThat(m -> m.getMovementType() == MovementType.MANUAL_ADJUSTMENT && m.getQuantity() == -3));
            verify(inventoryItemService).updateStock("item-1", 2);
        }
    }
}
