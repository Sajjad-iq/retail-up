package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.inventory.InventoryService;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.FilterRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.PagedResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.utils.dto.Currency;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class InventoryItemServiceIntegrationTest {

    @Autowired
    private InventoryItemService inventoryItemService;

    @MockitoBean
    private InventoryItemRepository inventoryItemRepository;

    @MockitoBean
    private InventoryService inventoryService;

    @MockitoBean
    private UserService userService;

    @MockitoBean
    private SystemSettingsService systemSettingsService;

    private User testUser;
    private Inventory testInventory;

    @BeforeEach
    void setUp() {
        testUser = User.builder()
                .id("user-1234567890123456")
                .name("Test User")
                .phone("1234567890")
                .email("test@example.com")
                .password("encodedPassword")
                .status(UserStatus.ACTIVE)
                .accountType(AccountType.USER)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        testInventory = Inventory.builder()
                .id("inv-1234567890123456")
                .name("Main Inventory")
                .description("Primary stock location")
                .location("A1")
                .isActive(true)
                .organizationId("org-123")
                .createdBy(testUser)
                .build();
    }

    private CreateInventoryItemRequest buildCreateRequest() {
        CreateInventoryItemRequest req = new CreateInventoryItemRequest();
        req.setUserId(testUser.getId());
        req.setInventoryId(testInventory.getId());
        req.setName("Laptop Computer");
        req.setDescription("High-performance laptop");
        req.setSku("SKU-001");
        req.setProductCode("PRD-001");
        req.setBarcode("1234567890123");
        req.setCategory("Electronics");
        req.setBrand("TechCorp");
        req.setUnit(Unit.PIECES);
        req.setWeight(new BigDecimal("1.25"));
        req.setDimensions("30x20x2");
        req.setColor("Black");
        req.setSize("15-inch");
        req.setCurrentStock(10);
        req.setMinimumStock(2);
        req.setMaximumStock(50);
        req.setCostPrice(new BigDecimal("800.00"));
        req.setSellingPrice(new BigDecimal("1200.00"));
        req.setDiscountPrice(new BigDecimal("1000.00"));
        req.setDiscountStartDate(LocalDateTime.now().minusDays(1));
        req.setDiscountEndDate(LocalDateTime.now().plusDays(7));
        req.setSupplierName("TechSupplier");
        req.setIsPerishable(false);
        req.setExpiryDate(null);
        return req;
    }

    private InventoryItem buildInventoryItemSaved(String id) {
        return InventoryItem.builder()
                .id(id)
                .name("Laptop Computer")
                .description("High-performance laptop")
                .sku("SKU-001")
                .productCode("PRD-001")
                .barcode("1234567890123")
                .category("Electronics")
                .brand("TechCorp")
                .unit(Unit.PIECES)
                .weight(new BigDecimal("1.25"))
                .dimensions("30x20x2")
                .color("Black")
                .size("15-inch")
                .currentStock(10)
                .minimumStock(2)
                .maximumStock(50)
                // Money values are set by service before save; repository returns saved entity
                .discountPrice(new BigDecimal("1000.00"))
                .discountStartDate(LocalDateTime.now().minusDays(1))
                .discountEndDate(LocalDateTime.now().plusDays(7))
                .supplierName("TechSupplier")
                .isPerishable(false)
                .expiryDate(null)
                .inventoryId(testInventory.getId())
                .isActive(true)
                .createdBy(testUser)
                .inventory(testInventory)
                .build();
    }

    @Nested
    @DisplayName("Create Inventory Item Integration Tests")
    class CreateInventoryItemIntegrationTests {

        @Test
        @DisplayName("Complete inventory item creation flow should work end-to-end")
        void completeInventoryItemCreationFlow_ShouldWorkEndToEnd() {
            // Given
            CreateInventoryItemRequest request = buildCreateRequest();
            InventoryItem saved = buildInventoryItemSaved("item-123");

            when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
            when(userService.getUserById(testUser.getId())).thenReturn(testUser);
            when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId())).thenReturn(false);
            when(inventoryItemRepository.existsByBarcodeAndInventoryId("1234567890123", testInventory.getId()))
                    .thenReturn(false);
            when(systemSettingsService.getSystemSettings("org-123"))
                    .thenReturn(SystemSetting.builder().id("sys-1").organizationId("org-123").currency("EUR").build());
            when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

            // When
            InventoryItem result = inventoryItemService.createInventoryItem(request);

            // Then
            assertNotNull(result);
            assertEquals("item-123", result.getId());
            assertEquals("Laptop Computer", result.getName());
            verify(inventoryItemRepository).existsBySkuAndInventoryId("SKU-001", testInventory.getId());
            verify(inventoryItemRepository).existsByBarcodeAndInventoryId("1234567890123", testInventory.getId());
            verify(systemSettingsService).getSystemSettings("org-123");
            verify(inventoryItemRepository).save(argThat(
                    item -> item.getSellingPrice() != null && item.getSellingPrice().getCurrency() == Currency.EUR));
        }

        @Test
        @DisplayName("Creation with existing SKU should fail")
        void creationWithExistingSku_ShouldFail() {
            // Given
            CreateInventoryItemRequest request = buildCreateRequest();
            when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
            when(userService.getUserById(testUser.getId())).thenReturn(testUser);
            when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId())).thenReturn(true);

            // When & Then
            ConflictException ex = assertThrows(ConflictException.class,
                    () -> inventoryItemService.createInventoryItem(request));
            assertTrue(ex.getMessage().contains("already exists"));

            verify(inventoryItemRepository, never()).save(any());
        }

        @Test
        @DisplayName("Creation with existing barcode should fail")
        void creationWithExistingBarcode_ShouldFail() {
            // Given
            CreateInventoryItemRequest request = buildCreateRequest();
            when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
            when(userService.getUserById(testUser.getId())).thenReturn(testUser);
            when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId())).thenReturn(false);
            when(inventoryItemRepository.existsByBarcodeAndInventoryId("1234567890123", testInventory.getId()))
                    .thenReturn(true);

            // When & Then
            ConflictException ex = assertThrows(ConflictException.class,
                    () -> inventoryItemService.createInventoryItem(request));
            assertTrue(ex.getMessage().contains("already exists"));

            verify(inventoryItemRepository, never()).save(any());
        }

        @Test
        @DisplayName("Creation with non-existent user should wrap in BadRequestException")
        void creationWithNonExistentUser_ShouldWrapInBadRequestException() {
            // Given
            CreateInventoryItemRequest request = buildCreateRequest();
            when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
            when(userService.getUserById(testUser.getId())).thenReturn(null);
            when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString())).thenReturn(false);

            // When & Then
            BadRequestException ex = assertThrows(BadRequestException.class,
                    () -> inventoryItemService.createInventoryItem(request));

            assertTrue(ex.getMessage().contains("Failed to create inventory item"));
            assertTrue(ex.getMessage().contains("User not found"));

            verify(inventoryItemRepository, never()).save(any());
        }

        @Test
        @DisplayName("Creation with invalid currency should fallback to USD")
        void creationWithInvalidCurrency_ShouldFallbackToUSD() {
            // Given
            CreateInventoryItemRequest request = buildCreateRequest();
            InventoryItem saved = buildInventoryItemSaved("item-234");

            when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
            when(userService.getUserById(testUser.getId())).thenReturn(testUser);
            when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString())).thenReturn(false);
            when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString())).thenReturn(false);
            when(systemSettingsService.getSystemSettings("org-123"))
                    .thenReturn(
                            SystemSetting.builder().id("sys-1").organizationId("org-123").currency("INVALID").build());
            when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

            // When
            inventoryItemService.createInventoryItem(request);

            // Then
            verify(inventoryItemRepository).save(argThat(
                    item -> item.getSellingPrice() != null && item.getSellingPrice().getCurrency() == Currency.USD));
        }
    }

    @Nested
    @DisplayName("Update Inventory Item Integration Tests")
    class UpdateInventoryItemIntegrationTests {

        @Test
        @DisplayName("Update should work and set currencies based on system settings")
        void update_ShouldWork_AndResolveCurrency() {
            // Given
            InventoryItem existing = buildInventoryItemSaved("item-123");
            existing.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                    new BigDecimal("1200.00"), Currency.USD));
            existing.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                    new BigDecimal("800.00"), Currency.USD));

            UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
            update.setName("Updated Laptop");
            update.setBarcode("NEW-BC-999");
            update.setSellingPrice(new BigDecimal("1300.00"));
            update.setCostPrice(new BigDecimal("850.00"));

            when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
            when(systemSettingsService.getSystemSettings("org-123"))
                    .thenReturn(SystemSetting.builder().id("sys-1").organizationId("org-123").currency("GBP").build());
            when(inventoryItemRepository.existsByBarcodeAndInventoryId("NEW-BC-999", existing.getInventoryId()))
                    .thenReturn(false);
            when(inventoryItemRepository.save(any(InventoryItem.class))).thenAnswer(inv -> inv.getArgument(0));

            // When
            InventoryItem result = inventoryItemService.updateInventoryItem("item-123", update);

            // Then
            assertEquals("Updated Laptop", result.getName());
            assertEquals("NEW-BC-999", result.getBarcode());
            assertEquals(new BigDecimal("1300.00"), result.getSellingPrice().getAmount());
            assertEquals(Currency.GBP, result.getSellingPrice().getCurrency());
            assertEquals(new BigDecimal("850.00"), result.getCostPrice().getAmount());
            assertEquals(Currency.GBP, result.getCostPrice().getCurrency());
        }

        @Test
        @DisplayName("Update with barcode conflict should fail")
        void updateWithBarcodeConflict_ShouldFail() {
            // Given
            InventoryItem existing = buildInventoryItemSaved("item-123");
            existing.setBarcode("OLD-BC");
            UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
            update.setBarcode("DUP-BC");

            when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
            when(inventoryItemRepository.existsByBarcodeAndInventoryId("DUP-BC", existing.getInventoryId()))
                    .thenReturn(true);

            // When & Then
            ConflictException ex = assertThrows(ConflictException.class,
                    () -> inventoryItemService.updateInventoryItem("item-123", update));
            assertTrue(ex.getMessage().contains("already exists"));
        }

        @Test
        @DisplayName("Update non-existent inventory item should fail")
        void updateNonExistent_ShouldFail() {
            when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
            assertThrows(NotFoundException.class,
                    () -> inventoryItemService.updateInventoryItem("missing", new UpdateInventoryItemRequest()));
        }
    }

    @Nested
    @DisplayName("Get Inventory Item Integration Tests")
    class GetInventoryItemIntegrationTests {
        @Test
        @DisplayName("Get by ID should work")
        void getById_ShouldWork() {
            InventoryItem existing = buildInventoryItemSaved("item-123");
            when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));

            InventoryItem result = inventoryItemService.getInventoryItemById("item-123");
            assertNotNull(result);
            assertEquals("item-123", result.getId());
        }

        @Test
        @DisplayName("Get by ID for missing item should fail")
        void getById_Missing_ShouldFail() {
            when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
            assertThrows(NotFoundException.class, () -> inventoryItemService.getInventoryItemById("missing"));
        }

        @Test
        @DisplayName("Get by SKU should work")
        void getBySku_ShouldWork() {
            InventoryItem existing = buildInventoryItemSaved("item-123");
            when(inventoryItemRepository.findBySkuAndInventoryId("SKU-001", testInventory.getId()))
                    .thenReturn(Optional.of(existing));

            InventoryItem result = inventoryItemService.getInventoryItemBySku("SKU-001", testInventory.getId());
            assertNotNull(result);
            assertEquals("item-123", result.getId());
        }

        @Test
        @DisplayName("Get by SKU for missing item should fail")
        void getBySku_Missing_ShouldFail() {
            when(inventoryItemRepository.findBySkuAndInventoryId(anyString(), anyString()))
                    .thenReturn(Optional.empty());
            assertThrows(NotFoundException.class,
                    () -> inventoryItemService.getInventoryItemBySku("NOPE", testInventory.getId()));
        }

        @Test
        @DisplayName("Get by barcode should work")
        void getByBarcode_ShouldWork() {
            InventoryItem existing = buildInventoryItemSaved("item-123");
            when(inventoryItemRepository.findByBarcodeAndInventoryId("1234567890123", testInventory.getId()))
                    .thenReturn(Optional.of(existing));

            InventoryItem result = inventoryItemService.getInventoryItemByBarcode("1234567890123",
                    testInventory.getId());
            assertNotNull(result);
            assertEquals("item-123", result.getId());
        }

        @Test
        @DisplayName("Get by barcode for missing item should fail")
        void getByBarcode_Missing_ShouldFail() {
            when(inventoryItemRepository.findByBarcodeAndInventoryId(anyString(), anyString()))
                    .thenReturn(Optional.empty());
            assertThrows(NotFoundException.class,
                    () -> inventoryItemService.getInventoryItemByBarcode("NOPE", testInventory.getId()));
        }
    }

    @Nested
    @DisplayName("Delete and Count Integration Tests")
    class DeleteAndCountIntegrationTests {
        @Test
        @DisplayName("Delete should set isActive to false and save")
        void delete_ShouldSetInactive() {
            InventoryItem existing = buildInventoryItemSaved("item-123");
            existing.setIsActive(true);

            when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
            when(inventoryItemRepository.save(any(InventoryItem.class))).thenAnswer(inv -> inv.getArgument(0));

            inventoryItemService.deleteInventoryItem("item-123");

            verify(inventoryItemRepository).save(argThat(item -> Boolean.FALSE.equals(item.getIsActive())));
        }

        @Test
        @DisplayName("Get item count by inventory should delegate to repository")
        void getItemCount_ShouldDelegate() {
            when(inventoryItemRepository.countByInventoryId("inv-1")).thenReturn(42L);
            long count = inventoryItemService.getItemCountByInventory("inv-1");
            assertEquals(42L, count);
        }

        @Test
        @DisplayName("Get active item count by inventory should delegate to repository")
        void getActiveItemCount_ShouldDelegate() {
            when(inventoryItemRepository.countByInventoryIdAndIsActiveTrue("inv-1")).thenReturn(24L);
            long count = inventoryItemService.getActiveItemCountByInventory("inv-1");
            assertEquals(24L, count);
        }
    }

    @Nested
    @DisplayName("Pagination and Filtering Integration Tests")
    class PaginationAndFilteringIntegrationTests {
        @Test
        @DisplayName("filterItemsPaginated should return mapped paged response")
        void filterItemsPaginated_ShouldReturnPagedResponse() {
            // Given
            String inventoryId = testInventory.getId();
            FilterRequest filter = new FilterRequest();
            filter.setCategory("Electronics");
            filter.setSearchTerm("laptop");

            InventoryItem item1 = buildInventoryItemSaved("item-1");
            InventoryItem item2 = buildInventoryItemSaved("item-2");
            List<InventoryItem> content = Arrays.asList(item1, item2);
            Page<InventoryItem> page = new PageImpl<>(content, PageRequest.of(0, 2), 5);

            when(inventoryItemRepository.findWithFilters(
                    eq(inventoryId),
                    any(), any(), any(), any(), any(),
                    any(), any(), any(), any(),
                    any(), any(), any(), any(),
                    any(),
                    any(Pageable.class))).thenReturn(page);

            // When
            PagedResponse<InventoryItem> resp = inventoryItemService.filterItemsPaginated(
                    inventoryId, filter, 0, 2, "createdAt", "desc");

            // Then
            assertNotNull(resp);
            assertEquals(2, resp.getContent().size());
            assertEquals(0, resp.getPage());
            assertEquals(2, resp.getSize());
            assertEquals(5, resp.getTotalElements());
            assertEquals(3, resp.getTotalPages());
            assertTrue(resp.isHasNext());
            assertTrue(resp.isFirst());
        }

        @Test
        @DisplayName("updateStock should update current stock")
        void updateStock_ShouldWork() {
            InventoryItem existing = buildInventoryItemSaved("item-123");
            existing.setCurrentStock(5);

            when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
            when(inventoryItemRepository.save(any(InventoryItem.class))).thenAnswer(inv -> inv.getArgument(0));

            InventoryItem updated = inventoryItemService.updateStock("item-123", 20);
            assertEquals(20, updated.getCurrentStock());
        }

        @Test
        @DisplayName("updateStock on missing item should fail")
        void updateStock_Missing_ShouldFail() {
            when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
            assertThrows(NotFoundException.class, () -> inventoryItemService.updateStock("missing", 10));
        }
    }
}