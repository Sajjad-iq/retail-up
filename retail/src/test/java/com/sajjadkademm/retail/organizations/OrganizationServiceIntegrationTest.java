package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;
import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Integration tests for OrganizationService that test the complete flow
 * including service interactions and business logic validation.
 */
@SpringBootTest
@ActiveProfiles("test")
@Transactional
class OrganizationServiceIntegrationTest {

    @Autowired
    private OrganizationService organizationService;

    @MockitoBean
    private OrganizationRepository organizationRepository;

    @MockitoBean
    private UserService userService;

    @MockitoBean
    private InventorySettingsService inventorySettingsService;

    @MockitoBean
    private POSSettingsService posSettingsService;

    @MockitoBean
    private SystemSettingsService systemSettingsService;

    private User testUser;
    private Organization testOrganization;
    private CreateOrganizationRequest createRequest;
    private UpdateOrganizationRequest updateRequest;

    @BeforeEach
    void setUp() {
        testUser = User.builder()
                .id("user-123")
                .name("Test User")
                .phone("1234567890")
                .email("test@example.com")
                .password("encodedPassword")
                .status(UserStatus.ACTIVE)
                .accountType(AccountType.USER)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        testOrganization = Organization.builder()
                .id("org-123")
                .name("Test Organization")
                .domain("test.com")
                .description("Test organization description")
                .address("123 Test Street, Test City")
                .phone("9876543210")
                .createdBy(testUser)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        createRequest = new CreateOrganizationRequest(
                "user-123",
                "New Organization",
                "neworg.com",
                "New organization description",
                "456 New Street, New City",
                "5555555555",
                "contact@neworg.com"
        );

        updateRequest = new UpdateOrganizationRequest(
                "Updated Organization",
                "Updated organization description",
                "789 Updated Street, Updated City"
        );
    }

    @Nested
    @DisplayName("Create Organization Integration Tests")
    class CreateOrganizationIntegrationTests {

        @Test
        @DisplayName("Complete organization creation flow should work end-to-end")
        void completeOrganizationCreationFlow_ShouldWorkEndToEnd() {
            // Given
            Organization savedOrganization = Organization.builder()
                    .id("new-org-456")
                    .name("New Organization")
                    .domain("neworg.com")
                    .description("New organization description")
                    .address("456 New Street, New City")
                    .phone("5555555555")
                    .createdBy(testUser)
                    .build();

            when(organizationRepository.existsByPhone("5555555555")).thenReturn(false);
            when(organizationRepository.existsByDomain("neworg.com")).thenReturn(false);
            when(userService.getUserById("user-123")).thenReturn(testUser);
            when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
            when(inventorySettingsService.createAndSaveDefaultInventorySettings("new-org-456", "user-123"))
                    .thenReturn(InventorySetting.builder().id("inv-123").organizationId("new-org-456").build());
            when(posSettingsService.createAndSaveDefaultPOSSettings("new-org-456", "user-123"))
                    .thenReturn(POSSetting.builder().id("pos-123").organizationId("new-org-456").build());
            when(systemSettingsService.createAndSaveDefaultSystemSettings("new-org-456", "user-123"))
                    .thenReturn(SystemSetting.builder().id("sys-123").organizationId("new-org-456").build());

            // When
            Organization result = organizationService.createOrganization(createRequest);

            // Then
            assertNotNull(result);
            assertEquals("new-org-456", result.getId());
            assertEquals("New Organization", result.getName());
            assertEquals("neworg.com", result.getDomain());
            assertEquals("New organization description", result.getDescription());
            assertEquals("456 New Street, New City", result.getAddress());
            assertEquals("5555555555", result.getPhone());
            assertEquals(testUser, result.getCreatedBy());

            // Verify the complete flow
            verify(organizationRepository).existsByPhone("5555555555");
            verify(organizationRepository).existsByDomain("neworg.com");
            verify(userService).getUserById("user-123");
            verify(organizationRepository).save(argThat(org -> 
                org.getName().equals("New Organization") &&
                org.getDomain().equals("neworg.com") &&
                org.getDescription().equals("New organization description") &&
                org.getAddress().equals("456 New Street, New City") &&
                org.getPhone().equals("5555555555") &&
                org.getCreatedBy().equals(testUser)
            ));
            verify(inventorySettingsService).createAndSaveDefaultInventorySettings("new-org-456", "user-123");
            verify(posSettingsService).createAndSaveDefaultPOSSettings("new-org-456", "user-123");
            verify(systemSettingsService).createAndSaveDefaultSystemSettings("new-org-456", "user-123");
        }

        @Test
        @DisplayName("Organization creation with existing phone should fail")
        void organizationCreationWithExistingPhone_ShouldFail() {
            // Given
            when(organizationRepository.existsByPhone("5555555555")).thenReturn(true);

            // When & Then
            ConflictException exception = assertThrows(ConflictException.class,
                    () -> organizationService.createOrganization(createRequest));

            assertEquals("Organization with phone 5555555555 already exists", exception.getMessage());

            // Verify no further processing
            verify(organizationRepository).existsByPhone("5555555555");
            verify(organizationRepository, never()).existsByDomain(anyString());
            verify(userService, never()).getUserById(anyString());
            verify(organizationRepository, never()).save(any(Organization.class));
            verify(inventorySettingsService, never()).createAndSaveDefaultInventorySettings(anyString(), anyString());
        }

        @Test
        @DisplayName("Organization creation with existing domain should fail")
        void organizationCreationWithExistingDomain_ShouldFail() {
            // Given
            when(organizationRepository.existsByPhone("5555555555")).thenReturn(false);
            when(organizationRepository.existsByDomain("neworg.com")).thenReturn(true);

            // When & Then
            ConflictException exception = assertThrows(ConflictException.class,
                    () -> organizationService.createOrganization(createRequest));

            assertEquals("Organization with domain neworg.com already exists", exception.getMessage());

            // Verify partial flow execution
            verify(organizationRepository).existsByPhone("5555555555");
            verify(organizationRepository).existsByDomain("neworg.com");
            verify(userService, never()).getUserById(anyString());
            verify(organizationRepository, never()).save(any(Organization.class));
        }

        @Test
        @DisplayName("Organization creation with non-existent user should fail")
        void organizationCreationWithNonExistentUser_ShouldFail() {
            // Given
            when(organizationRepository.existsByPhone("5555555555")).thenReturn(false);
            when(organizationRepository.existsByDomain("neworg.com")).thenReturn(false);
            when(userService.getUserById("user-123")).thenReturn(null);

            // When & Then
            BadRequestException exception = assertThrows(BadRequestException.class,
                    () -> organizationService.createOrganization(createRequest));

            assertTrue(exception.getMessage().contains("Failed to create organization"));
            assertTrue(exception.getMessage().contains("User not found with ID: user-123"));

            // Verify partial flow execution
            verify(organizationRepository).existsByPhone("5555555555");
            verify(organizationRepository).existsByDomain("neworg.com");
            verify(userService).getUserById("user-123");
            verify(organizationRepository, never()).save(any(Organization.class));
        }

        @Test
        @DisplayName("Organization creation with settings service failure should rollback transaction")
        void organizationCreationWithSettingsServiceFailure_ShouldRollbackTransaction() {
            // Given
            Organization savedOrganization = Organization.builder()
                    .id("new-org-456")
                    .name("New Organization")
                    .domain("neworg.com")
                    .description("New organization description")
                    .address("456 New Street, New City")
                    .phone("5555555555")
                    .createdBy(testUser)
                    .build();

            when(organizationRepository.existsByPhone("5555555555")).thenReturn(false);
            when(organizationRepository.existsByDomain("neworg.com")).thenReturn(false);
            when(userService.getUserById("user-123")).thenReturn(testUser);
            when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
            doThrow(new RuntimeException("Settings creation failed"))
                    .when(inventorySettingsService).createAndSaveDefaultInventorySettings("new-org-456", "user-123");

            // When & Then
            BadRequestException exception = assertThrows(BadRequestException.class,
                    () -> organizationService.createOrganization(createRequest));

            assertTrue(exception.getMessage().contains("Failed to create organization"));
            assertTrue(exception.getMessage().contains("Settings creation failed"));

            // Verify flow execution up to failure point
            verify(organizationRepository).existsByPhone("5555555555");
            verify(organizationRepository).existsByDomain("neworg.com");
            verify(userService).getUserById("user-123");
            verify(organizationRepository).save(any(Organization.class));
            verify(inventorySettingsService).createAndSaveDefaultInventorySettings("new-org-456", "user-123");
        }
    }

    @Nested
    @DisplayName("Update Organization Integration Tests")
    class UpdateOrganizationIntegrationTests {

        @Test
        @DisplayName("Organization update should work end-to-end")
        void organizationUpdate_ShouldWorkEndToEnd() {
            // Given
            Organization updatedOrganization = Organization.builder()
                    .id("org-123")
                    .name("Updated Organization")
                    .domain("test.com") // domain should not change
                    .description("Updated organization description")
                    .address("789 Updated Street, Updated City")
                    .phone("9876543210") // phone should not change
                    .createdBy(testUser)
                    .build();

            when(organizationRepository.findById("org-123")).thenReturn(Optional.of(testOrganization));
            when(organizationRepository.save(any(Organization.class))).thenReturn(updatedOrganization);

            // When
            Organization result = organizationService.updateOrganization("org-123", updateRequest);

            // Then
            assertNotNull(result);
            assertEquals("org-123", result.getId());
            assertEquals("Updated Organization", result.getName());
            assertEquals("Updated organization description", result.getDescription());
            assertEquals("789 Updated Street, Updated City", result.getAddress());
            assertEquals("test.com", result.getDomain()); // unchanged
            assertEquals("9876543210", result.getPhone()); // unchanged

            // Verify interactions
            verify(organizationRepository).findById("org-123");
            verify(organizationRepository).save(argThat(org -> 
                org.getName().equals("Updated Organization") &&
                org.getDescription().equals("Updated organization description") &&
                org.getAddress().equals("789 Updated Street, Updated City")
            ));
        }

        @Test
        @DisplayName("Update non-existent organization should fail")
        void updateNonExistentOrganization_ShouldFail() {
            // Given
            when(organizationRepository.findById("nonexistent")).thenReturn(Optional.empty());

            // When & Then
            NotFoundException exception = assertThrows(NotFoundException.class,
                    () -> organizationService.updateOrganization("nonexistent", updateRequest));

            assertEquals("Organization not found with ID: nonexistent", exception.getMessage());

            // Verify interactions
            verify(organizationRepository).findById("nonexistent");
            verify(organizationRepository, never()).save(any(Organization.class));
        }
    }

    @Nested
    @DisplayName("Get Organization Integration Tests")
    class GetOrganizationIntegrationTests {

        @Test
        @DisplayName("Get organization by ID should work correctly")
        void getOrganizationById_ShouldWorkCorrectly() {
            // Given
            when(organizationRepository.findById("org-123")).thenReturn(Optional.of(testOrganization));

            // When
            Organization result = organizationService.getOrganizationById("org-123");

            // Then
            assertNotNull(result);
            assertEquals("org-123", result.getId());
            assertEquals("Test Organization", result.getName());
            assertEquals("test.com", result.getDomain());
            assertEquals("Test organization description", result.getDescription());

            // Verify interactions
            verify(organizationRepository).findById("org-123");
        }

        @Test
        @DisplayName("Get non-existent organization should fail")
        void getNonExistentOrganization_ShouldFail() {
            // Given
            when(organizationRepository.findById("nonexistent")).thenReturn(Optional.empty());

            // When & Then
            NotFoundException exception = assertThrows(NotFoundException.class,
                    () -> organizationService.getOrganizationById("nonexistent"));

            assertEquals("Organization not found with ID: nonexistent", exception.getMessage());

            // Verify interactions
            verify(organizationRepository).findById("nonexistent");
        }

        @Test
        @DisplayName("Get all organizations should work correctly")
        void getAllOrganizations_ShouldWorkCorrectly() {
            // Given
            Organization org2 = Organization.builder()
                    .id("org-456")
                    .name("Second Organization")
                    .domain("second.com")
                    .description("Second organization description")
                    .address("456 Second Street")
                    .phone("1111111111")
                    .createdBy(testUser)
                    .build();

            List<Organization> organizations = Arrays.asList(testOrganization, org2);
            when(organizationRepository.findAll()).thenReturn(organizations);

            // When
            List<Organization> result = organizationService.getAllOrganizations();

            // Then
            assertNotNull(result);
            assertEquals(2, result.size());
            assertEquals("org-123", result.get(0).getId());
            assertEquals("org-456", result.get(1).getId());

            // Verify interactions
            verify(organizationRepository).findAll();
        }

        @Test
        @DisplayName("Get all organizations with empty result should work correctly")
        void getAllOrganizationsWithEmptyResult_ShouldWorkCorrectly() {
            // Given
            when(organizationRepository.findAll()).thenReturn(Collections.emptyList());

            // When
            List<Organization> result = organizationService.getAllOrganizations();

            // Then
            assertNotNull(result);
            assertTrue(result.isEmpty());

            // Verify interactions
            verify(organizationRepository).findAll();
        }
    }

    @Nested
    @DisplayName("Search Organizations Integration Tests")
    class SearchOrganizationsIntegrationTests {

        @Test
        @DisplayName("Search organizations should work correctly")
        void searchOrganizations_ShouldWorkCorrectly() {
            // Given
            String searchTerm = "Test";
            List<Organization> searchResults = Arrays.asList(testOrganization);
            when(organizationRepository.searchOrganizations("Test")).thenReturn(searchResults);

            // When
            List<Organization> result = organizationService.searchOrganizations(searchTerm);

            // Then
            assertNotNull(result);
            assertEquals(1, result.size());
            assertEquals("org-123", result.get(0).getId());
            assertEquals("Test Organization", result.get(0).getName());

            // Verify interactions
            verify(organizationRepository).searchOrganizations("Test");
        }

        @Test
        @DisplayName("Search organizations with trimmed search term should work correctly")
        void searchOrganizationsWithTrimmedSearchTerm_ShouldWorkCorrectly() {
            // Given
            String searchTerm = "  Test  ";
            List<Organization> searchResults = Arrays.asList(testOrganization);
            when(organizationRepository.searchOrganizations("Test")).thenReturn(searchResults);

            // When
            List<Organization> result = organizationService.searchOrganizations(searchTerm);

            // Then
            assertNotNull(result);
            assertEquals(1, result.size());

            // Verify interactions with trimmed term
            verify(organizationRepository).searchOrganizations("Test");
        }

        @Test
        @DisplayName("Search organizations with empty search term should fail")
        void searchOrganizationsWithEmptySearchTerm_ShouldFail() {
            // When & Then
            BadRequestException exception = assertThrows(BadRequestException.class,
                    () -> organizationService.searchOrganizations(""));

            assertEquals("Search term cannot be empty", exception.getMessage());

            // Verify no repository interaction
            verify(organizationRepository, never()).searchOrganizations(anyString());
        }

        @Test
        @DisplayName("Search organizations with null search term should fail")
        void searchOrganizationsWithNullSearchTerm_ShouldFail() {
            // When & Then
            BadRequestException exception = assertThrows(BadRequestException.class,
                    () -> organizationService.searchOrganizations(null));

            assertEquals("Search term cannot be empty", exception.getMessage());

            // Verify no repository interaction
            verify(organizationRepository, never()).searchOrganizations(anyString());
        }

        @Test
        @DisplayName("Search organizations with whitespace-only search term should fail")
        void searchOrganizationsWithWhitespaceOnlySearchTerm_ShouldFail() {
            // When & Then
            BadRequestException exception = assertThrows(BadRequestException.class,
                    () -> organizationService.searchOrganizations("   "));

            assertEquals("Search term cannot be empty", exception.getMessage());

            // Verify no repository interaction
            verify(organizationRepository, never()).searchOrganizations(anyString());
        }

        @Test
        @DisplayName("Search organizations with no results should return empty list")
        void searchOrganizationsWithNoResults_ShouldReturnEmptyList() {
            // Given
            String searchTerm = "NonExistent";
            when(organizationRepository.searchOrganizations("NonExistent")).thenReturn(Collections.emptyList());

            // When
            List<Organization> result = organizationService.searchOrganizations(searchTerm);

            // Then
            assertNotNull(result);
            assertTrue(result.isEmpty());

            // Verify interactions
            verify(organizationRepository).searchOrganizations("NonExistent");
        }
    }

    @Nested
    @DisplayName("Organization Existence Check Integration Tests")
    class OrganizationExistenceCheckIntegrationTests {

        @Test
        @DisplayName("Organization exists by domain check should work correctly")
        void organizationExistsByDomainCheck_ShouldWorkCorrectly() {
            // Given
            when(organizationRepository.existsByDomain("test.com")).thenReturn(true);
            when(organizationRepository.existsByDomain("nonexistent.com")).thenReturn(false);

            // When & Then
            assertTrue(organizationService.organizationExistsByDomain("test.com"));
            assertFalse(organizationService.organizationExistsByDomain("nonexistent.com"));

            // Verify interactions
            verify(organizationRepository).existsByDomain("test.com");
            verify(organizationRepository).existsByDomain("nonexistent.com");
        }
    }
}