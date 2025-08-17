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
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.InventoryMovement.dto.ReferenceType;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.UserRepository;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.utils.dto.Currency;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;

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
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
@DirtiesContext
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
        private UserRepository userRepository;

        @MockitoBean
        private SystemSettingsService systemSettingsService;

        @MockitoBean
        private InventoryMovementService inventoryMovementService;

        @MockitoBean
        private OrganizationService organizationService;

        private User testUser;
        private Inventory testInventory;
        private Organization testOrganization;

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

                // Provide an active organization for validator checks
                testOrganization = Organization.builder()
                                .id("org-123")
                                .name("Test Org")
                                .domain("org.test")
                                .address("addr")
                                .phone("000")
                                .createdBy(testUser)
                                .build();
                when(organizationService.getOrganizationById("org-123")).thenReturn(testOrganization);

                // Mock user service to return the test user
                when(userService.getUserById(testUser.getId())).thenReturn(testUser);

                // Mock user repository to return the test user (for validation utilities)
                when(userRepository.findById(testUser.getId())).thenReturn(Optional.of(testUser));

                // Mock system settings service
                SystemSetting systemSetting = SystemSetting.builder()
                                .organizationId("org-123")
                                .currency(Currency.USD)
                                .build();
                when(systemSettingsService.getSystemSettings("org-123")).thenReturn(systemSetting);
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
                req.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(new BigDecimal("800.00"),
                                Currency.USD));
                req.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                new BigDecimal("1200.00"), Currency.USD));
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
                        when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId("1234567890123",
                                        testInventory.getId()))
                                        .thenReturn(false);

                        when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

                        // When
                        InventoryItem result = inventoryItemService.createInventoryItem(request);

                        // Then
                        assertNotNull(result);
                        assertEquals("item-123", result.getId());
                        assertEquals("Laptop Computer", result.getName());
                        verify(inventoryItemRepository).existsBySkuAndInventoryId("SKU-001", testInventory.getId());
                        verify(inventoryItemRepository).existsByBarcodeAndInventoryId("1234567890123",
                                        testInventory.getId());
                        verify(inventoryItemRepository).save(argThat(
                                        item -> item.getSellingPrice() != null
                                                        && item.getSellingPrice().getCurrency() == Currency.USD));
                        // Verify initial movement recorded
                        verify(inventoryMovementService).recordStockIn(
                                        testUser,
                                        saved,
                                        10,
                                        "Initial stock on item creation",
                                        ReferenceType.CREATION,
                                        "item-123");
                }

                @Test
                @DisplayName("Creation with zero initial stock should record movement")
                void creationWithZeroInitialStock_ShouldRecordMovement() {
                        // Given
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCurrentStock(0);
                        InventoryItem saved = buildInventoryItemSaved("item-456");

                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), eq(testInventory.getId())))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(),
                                        eq(testInventory.getId())))
                                        .thenReturn(false);

                        when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

                        // When
                        inventoryItemService.createInventoryItem(request);

                        // Then - movement should be recorded for zero stock
                        verify(inventoryMovementService).recordStockIn(
                                        eq(testUser),
                                        eq(saved),
                                        eq(0),
                                        eq("Initial stock on item creation"),
                                        eq(ReferenceType.CREATION),
                                        eq(saved.getId()));
                }

                @Test
                @DisplayName("Creation with existing SKU should fail")
                void creationWithExistingSku_ShouldFail() {
                        // Given
                        CreateInventoryItemRequest request = buildCreateRequest();
                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId()))
                                        .thenReturn(true);

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
                        when(inventoryItemRepository.existsBySkuAndInventoryId("SKU-001", testInventory.getId()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId("1234567890123",
                                        testInventory.getId()))
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
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);

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
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(systemSettingsService.getSystemSettings("org-123"))
                                        .thenReturn(
                                                        SystemSetting.builder().id("sys-1").organizationId("org-123")
                                                                        .currency(Currency.USD).build());
                        when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

                        // When
                        inventoryItemService.createInventoryItem(request);

                        // Then
                        verify(inventoryItemRepository).save(argThat(
                                        item -> item.getSellingPrice() != null
                                                        && item.getSellingPrice().getCurrency() == Currency.USD));
                }
        }

        @Nested
        @DisplayName("Update Inventory Item Integration Tests")
        class UpdateInventoryItemIntegrationTests {

                @Test
                @DisplayName("Update should work and preserve currencies from request")
                void update_ShouldWork_AndPreserveCurrency() {
                        // Given
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        existing.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("1200.00"), Currency.USD));
                        existing.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("800.00"), Currency.USD));

                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setName("Updated Laptop");
                        update.setBarcode("NEW-BC-999");
                        update.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("1300.00"), Currency.USD));
                        update.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("850.00"), Currency.USD));

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));

                        when(inventoryItemRepository.existsByBarcodeAndInventoryId("NEW-BC-999",
                                        existing.getInventoryId()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));

                        // When
                        InventoryItem result = inventoryItemService.updateInventoryItem("item-123", update);

                        // Then
                        assertEquals("Updated Laptop", result.getName());
                        assertEquals("NEW-BC-999", result.getBarcode());
                        assertEquals(new BigDecimal("1300.00"), result.getSellingPrice().getAmount());
                        assertEquals(Currency.USD, result.getSellingPrice().getCurrency());
                        assertEquals(new BigDecimal("850.00"), result.getCostPrice().getAmount());
                        assertEquals(Currency.USD, result.getCostPrice().getCurrency());
                        // No stock change requested => no adjustment recorded
                        verify(inventoryMovementService, never()).recordAdjustmentToTarget(any(), any(), anyInt(),
                                        any(), any(),
                                        any());
                }

                @Test
                @DisplayName("Update with barcode conflict should fail")
                void updateWithBarcodeConflict_ShouldFail() {
                        // Given
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        existing.setBarcode("OLD-BC");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
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
                @DisplayName("Update with stock change should record adjustment movement")
                void updateWithStockChange_ShouldRecordAdjustmentMovement() {
                        // Given
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        existing.setCurrentStock(5);

                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(12);

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);

                        // When
                        InventoryItem result = inventoryItemService.updateInventoryItem("item-123", update);

                        // Then
                        assertEquals(12, result.getCurrentStock());
                        verify(inventoryMovementService).recordAdjustmentToTarget(
                                        eq(testUser),
                                        any(InventoryItem.class),
                                        eq(12),
                                        eq("Stock increased via item update"),
                                        eq(ReferenceType.ADJUSTMENT),
                                        eq("item-123"));
                }

                @Test
                @DisplayName("Update with stock decrease should record adjustment movement out")
                void updateWithStockDecrease_ShouldRecordAdjustmentMovement() {
                        // Given
                        InventoryItem existing = buildInventoryItemSaved("item-789");
                        existing.setCurrentStock(10);

                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(2);

                        when(inventoryItemRepository.findById("item-789")).thenReturn(Optional.of(existing));
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);

                        // When
                        InventoryItem result = inventoryItemService.updateInventoryItem("item-789", update);

                        // Then
                        assertEquals(2, result.getCurrentStock());
                        verify(inventoryMovementService).recordAdjustmentToTarget(
                                        eq(testUser),
                                        any(InventoryItem.class),
                                        eq(2),
                                        eq("Stock decreased via item update"),
                                        eq(ReferenceType.ADJUSTMENT),
                                        eq("item-789"));
                }

                @Test
                @DisplayName("Update unexpected repository error should wrap into BadRequestException")
                void updateUnexpectedRepoError_ShouldWrapInBadRequest() {
                        // Given
                        InventoryItem existing = buildInventoryItemSaved("item-500");
                        when(inventoryItemRepository.findById("item-500")).thenReturn(Optional.of(existing));
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());

                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenThrow(new RuntimeException("DB down"));

                        // When & Then
                        BadRequestException ex = assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-500", update));
                        assertTrue(ex.getMessage().contains("Failed to update inventory item"));
                }

                @Test
                @DisplayName("Update non-existent inventory item should fail")
                void updateNonExistent_ShouldFail() {
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());

                        when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.updateInventoryItem("missing", update));
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
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.getInventoryItemById("missing"));
                }

                @Test
                @DisplayName("Get by SKU should work")
                void getBySku_ShouldWork() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        when(inventoryItemRepository.findBySkuAndInventoryId("SKU-001", testInventory.getId()))
                                        .thenReturn(Optional.of(existing));

                        InventoryItem result = inventoryItemService.getInventoryItemBySku("SKU-001",
                                        testInventory.getId());
                        assertNotNull(result);
                        assertEquals("item-123", result.getId());
                }

                @Test
                @DisplayName("Get by SKU for missing item should fail")
                void getBySku_Missing_ShouldFail() {
                        when(inventoryItemRepository.findBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.getInventoryItemBySku("NOPE",
                                                        testInventory.getId()));
                }

                @Test
                @DisplayName("Get by barcode should work")
                void getByBarcode_ShouldWork() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        when(inventoryItemRepository.findByBarcodeAndInventoryId("1234567890123",
                                        testInventory.getId()))
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
                                        () -> inventoryItemService.getInventoryItemByBarcode("NOPE",
                                                        testInventory.getId()));
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
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));

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

        }

        @Nested
        @DisplayName("Edge Cases and Error Handling Tests")
        class EdgeCasesAndErrorHandlingTests {

                @Test
                @DisplayName("Create with null name should fail validation")
                void createWithNullName_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setName(null);
                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);

                        // This should fail because name is required
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with duplicate SKU should fail with conflict")
                void createWithDuplicateSku_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(true);
                        assertThrows(ConflictException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with current stock exceeding maximum stock should fail business validation")
                void createWithCurrentStockExceedingMaxStock_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCurrentStock(15); // Set current stock higher than max stock
                        request.setMaximumStock(10); // Set max stock to trigger validation
                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenReturn(buildInventoryItemSaved("item-123"));

                        // This should fail because currentStock(15) > maxStock(10)
                        // The validation checks: if (maxStock != null && currentStock != null &&
                        // currentStock > maxStock)
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with cost price greater than selling price should fail")
                void createWithCostPriceGreaterThanSellingPrice_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("1500.00"), Currency.USD));
                        request.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("1200.00"), Currency.USD));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with non-existent inventory should fail")
                void createWithNonExistentInventory_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        when(inventoryService.getInventoryById(anyString()))
                                        .thenThrow(new NotFoundException("Inventory not found"));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with inactive user should fail")
                void createWithInactiveUser_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        User inactiveUser = User.builder()
                                        .id("inactive-user")
                                        .name("Inactive User")
                                        .status(UserStatus.INACTIVE)
                                        .build();
                        when(userService.getUserById(anyString())).thenReturn(inactiveUser);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Update with non-existent item should fail")
                void updateWithNonExistentItem_ShouldFail() {
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.updateInventoryItem("missing", update));
                }

                @Test
                @DisplayName("Update with null user ID should fail")
                void updateWithNullUserId_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(null);
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit
                        update.setCurrentStock(10); // Provide required current stock
                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(null)).thenReturn(null); // Mock user service to return null for
                                                                              // null userId
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because user ID is null
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Delete non-existent item should fail")
                void deleteNonExistentItem_ShouldFail() {
                        when(inventoryItemRepository.findById("missing")).thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.deleteInventoryItem("missing"));
                }

                @Test
                @DisplayName("Get by non-existent SKU should fail")
                void getByNonExistentSku_ShouldFail() {
                        when(inventoryItemRepository.findBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.getInventoryItemBySku("NOPE", "inv-1"));
                }

                @Test
                @DisplayName("Get by non-existent barcode should fail")
                void getByNonExistentBarcode_ShouldFail() {
                        when(inventoryItemRepository.findByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(Optional.empty());
                        assertThrows(NotFoundException.class,
                                        () -> inventoryItemService.getInventoryItemByBarcode("NOPE", "inv-1"));
                }

                @Test
                @DisplayName("Filter with invalid page number should use default")
                void filterWithInvalidPageNumber_ShouldUseDefault() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenReturn(new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1"))));

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, -1, 10, "createdAt", "desc");

                        assertEquals(0, result.getPage());
                }

                @Test
                @DisplayName("Filter with invalid page size should use default")
                void filterWithInvalidPageSize_ShouldUseDefault() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenAnswer(inv -> {
                                                Pageable pageable = inv.getArgument(15); // Pageable is the last
                                                                                         // argument
                                                return new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1")),
                                                                pageable, 1);
                                        });

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, 0, 0, "createdAt", "desc");

                        assertEquals(20, result.getSize());
                }

                @Test
                @DisplayName("Filter with very large page size should be capped")
                void filterWithVeryLargePageSize_ShouldBeCapped() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenAnswer(inv -> {
                                                Pageable pageable = inv.getArgument(15); // Pageable is the last
                                                                                         // argument
                                                return new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1")),
                                                                pageable, 1);
                                        });

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, 0, 1000, "createdAt", "desc");

                        assertEquals(100, result.getSize());
                }

                @Test
                @DisplayName("Create with empty name should fail validation")
                void createWithEmptyName_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setName("   ");
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null SKU should fail validation")
                void createWithNullSku_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setSku(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with duplicate barcode should fail with conflict")
                void createWithDuplicateBarcode_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(true);
                        assertThrows(ConflictException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with negative minimum stock should fail validation")
                void createWithNegativeMinStock_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setMinimumStock(-1);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with minimum stock greater than maximum stock should fail")
                void createWithMinStockGreaterThanMaxStock_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setMinimumStock(20);
                        request.setMaximumStock(10);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with negative weight should fail validation")
                void createWithNegativeWeight_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setWeight(new BigDecimal("-0.5"));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with discount price greater than selling price should fail")
                void createWithDiscountPriceGreaterThanSellingPrice_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setDiscountPrice(new BigDecimal("1500.00"));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with discount start date after end date should fail")
                void createWithDiscountStartAfterEnd_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setDiscountStartDate(LocalDateTime.now().plusDays(7));
                        request.setDiscountEndDate(LocalDateTime.now().plusDays(1));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with expiry date in the past should fail validation")
                void createWithExpiryDateInPast_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setIsPerishable(true);
                        request.setExpiryDate(LocalDate.now().minusDays(1));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null inventory ID should fail validation")
                void createWithNullInventoryId_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setInventoryId(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null user ID should fail validation")
                void createWithNullUserId_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setUserId(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null unit should fail validation")
                void createWithNullUnit_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setUnit(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null category should fail validation")
                void createWithNullCategory_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCategory(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null brand should fail validation")
                void createWithNullBrand_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setBrand(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null supplier name should fail validation")
                void createWithNullSupplierName_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setSupplierName(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null cost price should fail validation")
                void createWithNullCostPrice_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCostPrice(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null selling price should fail validation")
                void createWithNullSellingPrice_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setSellingPrice(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with mismatched currencies should fail validation")
                void createWithMismatchedCurrencies_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCostPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("800.00"), Currency.EUR));
                        request.setSellingPrice(new com.sajjadkademm.retail.inventory.InventoryItem.dto.Money(
                                        new BigDecimal("1200.00"), Currency.USD));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with null expiry date for perishable item should fail")
                void createWithNullExpiryDateForPerishable_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setIsPerishable(true);
                        request.setExpiryDate(null);
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with non-perishable item having expiry date should fail")
                void createWithNonPerishableHavingExpiryDate_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setIsPerishable(false);
                        request.setExpiryDate(LocalDate.now().plusDays(30));
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Create with zero stock should record movement")
                void createWithZeroStock_ShouldRecordMovement() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCurrentStock(0);
                        InventoryItem saved = buildInventoryItemSaved("item-123");

                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.save(any(InventoryItem.class))).thenReturn(saved);

                        inventoryItemService.createInventoryItem(request);

                        verify(inventoryMovementService).recordStockIn(
                                        eq(testUser), eq(saved), eq(0),
                                        eq("Initial stock on item creation"),
                                        eq(ReferenceType.CREATION), eq(saved.getId()));
                }

                @Test
                @DisplayName("Create with null stock should fail validation")
                void createWithNullStock_ShouldFail() {
                        CreateInventoryItemRequest request = buildCreateRequest();
                        request.setCurrentStock(null);

                        when(inventoryService.getInventoryById(testInventory.getId())).thenReturn(testInventory);
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryItemRepository.existsBySkuAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);
                        when(inventoryItemRepository.existsByBarcodeAndInventoryId(anyString(), anyString()))
                                        .thenReturn(false);

                        // This should fail because current stock is required
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.createInventoryItem(request));
                }

                @Test
                @DisplayName("Update with null stock should fail validation")
                void updateWithNullStock_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(null);
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because current stock is required
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Update with same stock should not record movement")
                void updateWithSameStock_ShouldNotRecordMovement() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        existing.setCurrentStock(10);
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(10);
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        inventoryItemService.updateInventoryItem("item-123", update);

                        verify(inventoryMovementService, never()).recordAdjustmentToTarget(
                                        any(), any(), anyInt(), anyString(), any(), anyString());
                }

                @Test
                @DisplayName("Update with negative stock should fail validation")
                void updateWithNegativeStock_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(-5);
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because current stock cannot be negative
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Update with very large stock should fail validation")
                void updateWithVeryLargeStock_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setCurrentStock(1000000);
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because currentStock(1000000) > maxStock(50)
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Update with invalid discount price should fail validation")
                void updateWithInvalidDiscountPrice_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setDiscountPrice(new BigDecimal("-50.00"));
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit
                        update.setCurrentStock(10); // Provide required current stock

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because discount price is negative
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Update with invalid discount dates should fail validation")
                void updateWithInvalidDiscountDates_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setDiscountPrice(new BigDecimal("50.00")); // Set discount price to trigger validation
                        update.setDiscountStartDate(LocalDateTime.now().plusDays(7));
                        update.setDiscountEndDate(LocalDateTime.now().plusDays(1));
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit
                        update.setCurrentStock(10); // Provide required current stock

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because discountStartDate is after discountEndDate
                        // The validation only happens when discountPrice is not null
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Update with discount dates but no discount price should work")
                void updateWithDiscountDatesButNoPrice_ShouldWork() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit
                        update.setCurrentStock(10); // Provide required current stock
                        // Don't set discount price, but set dates
                        update.setDiscountStartDate(LocalDateTime.now().plusDays(1));
                        update.setDiscountEndDate(LocalDateTime.now().plusDays(7));

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(inventoryItemRepository.save(any(InventoryItem.class)))
                                        .thenAnswer(inv -> inv.getArgument(0));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should work because the validation only checks dates when discountPrice
                        // is set
                        // But we can test that the dates are properly applied
                        InventoryItem result = inventoryItemService.updateInventoryItem("item-123", update);
                        assertNotNull(result);
                        assertEquals(update.getDiscountStartDate(), result.getDiscountStartDate());
                        assertEquals(update.getDiscountEndDate(), result.getDiscountEndDate());
                }

                @Test
                @DisplayName("Update with invalid expiry date should fail validation")
                void updateWithInvalidExpiryDate_ShouldFail() {
                        InventoryItem existing = buildInventoryItemSaved("item-123");
                        existing.setIsPerishable(true);
                        UpdateInventoryItemRequest update = new UpdateInventoryItemRequest();
                        update.setUserId(testUser.getId());
                        update.setExpiryDate(LocalDate.now().minusDays(1));
                        update.setName("Updated Name"); // Provide required name
                        update.setUnit(Unit.PIECES); // Provide required unit
                        update.setCurrentStock(10); // Provide required current stock

                        when(inventoryItemRepository.findById("item-123")).thenReturn(Optional.of(existing));
                        when(userService.getUserById(testUser.getId())).thenReturn(testUser);
                        when(inventoryService.getInventoryById(existing.getInventoryId())).thenReturn(testInventory);
                        when(organizationService.getOrganizationById(testInventory.getOrganizationId()))
                                        .thenReturn(testOrganization);

                        // This should fail because expiry date is in the past for a perishable item
                        assertThrows(BadRequestException.class,
                                        () -> inventoryItemService.updateInventoryItem("item-123", update));
                }

                @Test
                @DisplayName("Filter with null sort field should use default")
                void filterWithNullSortField_ShouldUseDefault() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenReturn(new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1"))));

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, 0, 10, null, "desc");

                        assertNotNull(result);
                }

                @Test
                @DisplayName("Filter with null sort direction should use default")
                void filterWithNullSortDirection_ShouldUseDefault() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenReturn(new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1"))));

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, 0, 10, "createdAt", null);

                        assertNotNull(result);
                }

                @Test
                @DisplayName("Filter with invalid sort direction should use default")
                void filterWithInvalidSortDirection_ShouldUseDefault() {
                        FilterRequest filter = new FilterRequest();
                        when(inventoryItemRepository.findWithFilters(anyString(), any(), any(), any(), any(), any(),
                                        any(), any(), any(), any(), any(), any(), any(), any(), any(),
                                        any(Pageable.class)))
                                        .thenReturn(new PageImpl<>(Arrays.asList(buildInventoryItemSaved("item-1"))));

                        PagedResponse<InventoryItem> result = inventoryItemService.filterItemsPaginated(
                                        "inv-1", filter, 0, 10, "createdAt", "invalid");

                        assertNotNull(result);
                }
        }
}