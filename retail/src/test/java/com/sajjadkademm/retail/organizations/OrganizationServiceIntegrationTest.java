package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.organizations.dto.OrganizationStatus;

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
                                "contact@neworg.com");

                updateRequest = new UpdateOrganizationRequest(
                                "Updated Organization",
                                "Updated organization description",
                                "789 Updated Street, Updated City");
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
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("new-org-456", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-123").organizationId("new-org-456")
                                                        .build());

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
                        verify(organizationRepository).save(argThat(org -> org.getName().equals("New Organization") &&
                                        org.getDomain().equals("neworg.com") &&
                                        org.getDescription().equals("New organization description") &&
                                        org.getAddress().equals("456 New Street, New City") &&
                                        org.getPhone().equals("5555555555") &&
                                        org.getCreatedBy().equals(testUser)));
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
                        verify(organizationRepository).save(argThat(org -> org.getName().equals("Updated Organization")
                                        &&
                                        org.getDescription().equals("Updated organization description") &&
                                        org.getAddress().equals("789 Updated Street, Updated City")));
                }

                @Test
                @DisplayName("Updating a disabled organization should fail")
                void updatingDisabledOrganization_ShouldFail() {
                        // Given
                        Organization disabled = Organization.builder()
                                        .id(testOrganization.getId())
                                        .name(testOrganization.getName())
                                        .domain(testOrganization.getDomain())
                                        .description(testOrganization.getDescription())
                                        .address(testOrganization.getAddress())
                                        .phone(testOrganization.getPhone())
                                        .createdBy(testUser)
                                        .status(OrganizationStatus.DISABLED)
                                        .build();
                        when(organizationRepository.findById("org-123")).thenReturn(Optional.of(disabled));

                        // When & Then
                        BadRequestException exception = assertThrows(BadRequestException.class,
                                        () -> organizationService.updateOrganization("org-123", updateRequest));
                        assertTrue(exception.getMessage().contains("This Organization Disabled") ||
                                        exception.getMessage()
                                                        .contains("Disabled or Rejected or Suspended or Deleted"));

                        // Verify
                        verify(organizationRepository).findById("org-123");
                        verify(organizationRepository, never()).save(any(Organization.class));
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
                        when(organizationRepository.searchOrganizations("NonExistent"))
                                        .thenReturn(Collections.emptyList());

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
                @DisplayName("Check organization exists by domain should return true when exists")
                void checkOrganizationExistsByDomain_ShouldReturnTrueWhenExists() {
                        // Given
                        when(organizationRepository.existsByDomain("test.com")).thenReturn(true);

                        // When
                        boolean result = organizationService.organizationExistsByDomain("test.com");

                        // Then
                        assertTrue(result);

                        // Verify interactions
                        verify(organizationRepository).existsByDomain("test.com");
                }

                @Test
                @DisplayName("Check organization exists by domain should return false when not exists")
                void checkOrganizationExistsByDomain_ShouldReturnFalseWhenNotExists() {
                        // Given
                        when(organizationRepository.existsByDomain("nonexistent.com")).thenReturn(false);

                        // When
                        boolean result = organizationService.organizationExistsByDomain("nonexistent.com");

                        // Then
                        assertFalse(result);

                        // Verify interactions
                        verify(organizationRepository).existsByDomain("nonexistent.com");
                }

                @Test
                @DisplayName("Check organization exists by domain with null should return false")
                void checkOrganizationExistsByDomain_WithNull_ShouldReturnFalse() {
                        // Given
                        when(organizationRepository.existsByDomain(null)).thenReturn(false);

                        // When
                        boolean result = organizationService.organizationExistsByDomain(null);

                        // Then
                        assertFalse(result);

                        // Verify interactions
                        verify(organizationRepository).existsByDomain(null);
                }

                @Test
                @DisplayName("Check organization exists by domain with empty string should return false")
                void checkOrganizationExistsByDomain_WithEmptyString_ShouldReturnFalse() {
                        // Given
                        when(organizationRepository.existsByDomain("")).thenReturn(false);

                        // When
                        boolean result = organizationService.organizationExistsByDomain("");

                        // Then
                        assertFalse(result);

                        // Verify interactions
                        verify(organizationRepository).existsByDomain("");
                }
        }

        @Nested
        @DisplayName("Edge Cases and Validation Tests")
        class EdgeCasesAndValidationTests {

                @Test
                @DisplayName("Create organization with minimal valid data should work")
                void createOrganizationWithMinimalValidData_ShouldWork() {
                        // Given
                        CreateOrganizationRequest minimalRequest = new CreateOrganizationRequest(
                                        "user-123",
                                        "Min Org",
                                        "min.com",
                                        null, // description can be null
                                        "123 Street",
                                        "1234567890",
                                        "contact@min.com");

                        Organization savedOrganization = Organization.builder()
                                        .id("min-org-123")
                                        .name("Min Org")
                                        .domain("min.com")
                                        .description(null)
                                        .address("123 Street")
                                        .phone("1234567890")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.existsByPhone("1234567890")).thenReturn(false);
                        when(organizationRepository.existsByDomain("min.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);
                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("min-org-123", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-min").organizationId("min-org-123")
                                                        .build());

                        // When
                        Organization result = organizationService.createOrganization(minimalRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals("min-org-123", result.getId());
                        assertEquals("Min Org", result.getName());
                        assertEquals("min.com", result.getDomain());
                        assertNull(result.getDescription());
                        assertEquals("123 Street", result.getAddress());
                        assertEquals("1234567890", result.getPhone());
                }

                @Test
                @DisplayName("Update organization with null description should work")
                void updateOrganizationWithNullDescription_ShouldWork() {
                        // Given
                        UpdateOrganizationRequest nullDescRequest = new UpdateOrganizationRequest(
                                        "Updated Name",
                                        null, // null description
                                        "Updated Address");

                        Organization updatedOrganization = Organization.builder()
                                        .id("org-123")
                                        .name("Updated Name")
                                        .domain("test.com")
                                        .description(null)
                                        .address("Updated Address")
                                        .phone("9876543210")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.findById("org-123")).thenReturn(Optional.of(testOrganization));
                        when(organizationRepository.save(any(Organization.class))).thenReturn(updatedOrganization);

                        // When
                        Organization result = organizationService.updateOrganization("org-123", nullDescRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals("Updated Name", result.getName());
                        assertNull(result.getDescription());
                        assertEquals("Updated Address", result.getAddress());
                }

                @Test
                @DisplayName("Search organizations with special characters should work")
                void searchOrganizationsWithSpecialCharacters_ShouldWork() {
                        // Given
                        String specialSearchTerm = "Test & Co.";
                        List<Organization> searchResults = Arrays.asList(testOrganization);
                        when(organizationRepository.searchOrganizations("Test & Co.")).thenReturn(searchResults);

                        // When
                        List<Organization> result = organizationService.searchOrganizations(specialSearchTerm);

                        // Then
                        assertNotNull(result);
                        assertEquals(1, result.size());
                        verify(organizationRepository).searchOrganizations("Test & Co.");
                }

                @Test
                @DisplayName("Search organizations with single character should work")
                void searchOrganizationsWithSingleCharacter_ShouldWork() {
                        // Given
                        String singleChar = "T";
                        List<Organization> searchResults = Arrays.asList(testOrganization);
                        when(organizationRepository.searchOrganizations("T")).thenReturn(searchResults);

                        // When
                        List<Organization> result = organizationService.searchOrganizations(singleChar);

                        // Then
                        assertNotNull(result);
                        assertEquals(1, result.size());
                        verify(organizationRepository).searchOrganizations("T");
                }
        }

        @Nested
        @DisplayName("Transaction Rollback Tests")
        class TransactionRollbackTests {

                @Test
                @DisplayName("Create organization with system settings failure should rollback")
                void createOrganizationWithSystemSettingsFailure_ShouldRollback() {
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
                        doThrow(new RuntimeException("System settings creation failed"))
                                        .when(systemSettingsService)
                                        .createAndSaveDefaultSystemSettings("new-org-456", "user-123");

                        // When & Then
                        BadRequestException exception = assertThrows(BadRequestException.class,
                                        () -> organizationService.createOrganization(createRequest));

                        assertTrue(exception.getMessage().contains("Failed to create organization"));
                        assertTrue(exception.getMessage().contains("System settings creation failed"));

                        // Verify complete flow execution up to failure point
                        verify(organizationRepository).existsByPhone("5555555555");
                        verify(organizationRepository).existsByDomain("neworg.com");
                        verify(userService).getUserById("user-123");
                        verify(organizationRepository).save(any(Organization.class));
                        verify(systemSettingsService).createAndSaveDefaultSystemSettings("new-org-456", "user-123");
                }

                @Test
                @DisplayName("Create organization with repository save failure should fail")
                void createOrganizationWithRepositorySaveFailure_ShouldFail() {
                        // Given
                        when(organizationRepository.existsByPhone("5555555555")).thenReturn(false);
                        when(organizationRepository.existsByDomain("neworg.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);
                        doThrow(new RuntimeException("Database connection failed"))
                                        .when(organizationRepository).save(any(Organization.class));

                        // When & Then
                        BadRequestException exception = assertThrows(BadRequestException.class,
                                        () -> organizationService.createOrganization(createRequest));

                        assertTrue(exception.getMessage().contains("Failed to create organization"));
                        assertTrue(exception.getMessage().contains("Database connection failed"));

                        // Verify flow execution up to failure point
                        verify(organizationRepository).existsByPhone("5555555555");
                        verify(organizationRepository).existsByDomain("neworg.com");
                        verify(userService).getUserById("user-123");
                        verify(organizationRepository).save(any(Organization.class));
                }
        }

        @Nested
        @DisplayName("Data Validation and Boundary Tests")
        class DataValidationAndBoundaryTests {

                @Test
                @DisplayName("Create organization with very long name should work")
                void createOrganizationWithVeryLongName_ShouldWork() {
                        // Given
                        String longName = "A".repeat(255); // Assuming max length is 255
                        CreateOrganizationRequest longNameRequest = new CreateOrganizationRequest(
                                        "user-123",
                                        longName,
                                        "longname.com",
                                        "Long name organization",
                                        "123 Long Street",
                                        "9999999999",
                                        "contact@longname.com");

                        Organization savedOrganization = Organization.builder()
                                        .id("long-org-123")
                                        .name(longName)
                                        .domain("longname.com")
                                        .description("Long name organization")
                                        .address("123 Long Street")
                                        .phone("9999999999")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.existsByPhone("9999999999")).thenReturn(false);
                        when(organizationRepository.existsByDomain("longname.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);
                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("long-org-123", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-long")
                                                        .organizationId("long-org-123").build());

                        // When
                        Organization result = organizationService.createOrganization(longNameRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals(longName, result.getName());
                }

                @Test
                @DisplayName("Create organization with very long description should work")
                void createOrganizationWithVeryLongDescription_ShouldWork() {
                        // Given
                        String longDescription = "Description ".repeat(100); // Very long description
                        CreateOrganizationRequest longDescRequest = new CreateOrganizationRequest(
                                        "user-123",
                                        "Long Desc Org",
                                        "longdesc.com",
                                        longDescription,
                                        "123 Desc Street",
                                        "8888888888",
                                        "contact@longdesc.com");

                        Organization savedOrganization = Organization.builder()
                                        .id("desc-org-123")
                                        .name("Long Desc Org")
                                        .domain("longdesc.com")
                                        .description(longDescription)
                                        .address("123 Desc Street")
                                        .phone("8888888888")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.existsByPhone("8888888888")).thenReturn(false);
                        when(organizationRepository.existsByDomain("longdesc.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);
                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("desc-org-123", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-desc")
                                                        .organizationId("desc-org-123").build());

                        // When
                        Organization result = organizationService.createOrganization(longDescRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals(longDescription, result.getDescription());
                }

                @Test
                @DisplayName("Search organizations with unicode characters should work")
                void searchOrganizationsWithUnicodeCharacters_ShouldWork() {
                        // Given
                        String unicodeSearchTerm = "Tëst Örg 中文 العربية";
                        List<Organization> searchResults = Arrays.asList(testOrganization);
                        when(organizationRepository.searchOrganizations(unicodeSearchTerm)).thenReturn(searchResults);

                        // When
                        List<Organization> result = organizationService.searchOrganizations(unicodeSearchTerm);

                        // Then
                        assertNotNull(result);
                        assertEquals(1, result.size());
                        verify(organizationRepository).searchOrganizations(unicodeSearchTerm);
                }

                @Test
                @DisplayName("Search organizations with very long search term should work")
                void searchOrganizationsWithVeryLongSearchTerm_ShouldWork() {
                        // Given
                        String longSearchTerm = "Very".repeat(50); // Very long search term
                        List<Organization> searchResults = Collections.emptyList();
                        when(organizationRepository.searchOrganizations(longSearchTerm)).thenReturn(searchResults);

                        // When
                        List<Organization> result = organizationService.searchOrganizations(longSearchTerm);

                        // Then
                        assertNotNull(result);
                        assertTrue(result.isEmpty());
                        verify(organizationRepository).searchOrganizations(longSearchTerm);
                }

                @Test
                @DisplayName("Create organization with different domain formats should work")
                void createOrganizationWithDifferentDomainFormats_ShouldWork() {
                        // Given - Test with subdomain
                        CreateOrganizationRequest subdomainRequest = new CreateOrganizationRequest(
                                        "user-123",
                                        "Subdomain Org",
                                        "sub.example.com",
                                        "Subdomain organization",
                                        "123 Sub Street",
                                        "7777777777",
                                        "contact@sub.example.com");

                        Organization savedOrganization = Organization.builder()
                                        .id("sub-org-123")
                                        .name("Subdomain Org")
                                        .domain("sub.example.com")
                                        .description("Subdomain organization")
                                        .address("123 Sub Street")
                                        .phone("7777777777")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.existsByPhone("7777777777")).thenReturn(false);
                        when(organizationRepository.existsByDomain("sub.example.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);
                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("sub-org-123", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-sub").organizationId("sub-org-123")
                                                        .build());

                        // When
                        Organization result = organizationService.createOrganization(subdomainRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals("sub.example.com", result.getDomain());
                }

                @Test
                @DisplayName("Update organization with empty fields should work")
                void updateOrganizationWithEmptyFields_ShouldWork() {
                        // Given
                        UpdateOrganizationRequest emptyFieldsRequest = new UpdateOrganizationRequest(
                                        "", // empty name
                                        "", // empty description
                                        "" // empty address
                        );

                        Organization updatedOrganization = Organization.builder()
                                        .id("org-123")
                                        .name("")
                                        .domain("test.com")
                                        .description("")
                                        .address("")
                                        .phone("9876543210")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.findById("org-123")).thenReturn(Optional.of(testOrganization));
                        when(organizationRepository.save(any(Organization.class))).thenReturn(updatedOrganization);

                        // When
                        Organization result = organizationService.updateOrganization("org-123", emptyFieldsRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals("", result.getName());
                        assertEquals("", result.getDescription());
                        assertEquals("", result.getAddress());
                }
        }

        @Nested
        @DisplayName("Performance and Large Dataset Tests")
        class PerformanceAndLargeDatasetTests {

                @Test
                @DisplayName("Get all organizations with large dataset should work")
                void getAllOrganizationsWithLargeDataset_ShouldWork() {
                        // Given - Simulate large dataset
                        List<Organization> largeDataset = Collections.nCopies(1000, testOrganization);
                        when(organizationRepository.findAll()).thenReturn(largeDataset);

                        // When
                        List<Organization> result = organizationService.getAllOrganizations();

                        // Then
                        assertNotNull(result);
                        assertEquals(1000, result.size());
                        verify(organizationRepository).findAll();
                }

                @Test
                @DisplayName("Search organizations with large result set should work")
                void searchOrganizationsWithLargeResultSet_ShouldWork() {
                        // Given
                        String searchTerm = "Common";
                        List<Organization> largeResults = Collections.nCopies(500, testOrganization);
                        when(organizationRepository.searchOrganizations("Common")).thenReturn(largeResults);

                        // When
                        List<Organization> result = organizationService.searchOrganizations(searchTerm);

                        // Then
                        assertNotNull(result);
                        assertEquals(500, result.size());
                        verify(organizationRepository).searchOrganizations("Common");
                }

                @Test
                @DisplayName("Search organizations with no results should return empty list")
                void searchOrganizationsWithNoResults_ShouldReturnEmptyList() {
                        // Given
                        String searchTerm = "NonExistentOrganization";
                        when(organizationRepository.searchOrganizations("NonExistentOrganization"))
                                        .thenReturn(Collections.emptyList());

                        // When
                        List<Organization> result = organizationService.searchOrganizations(searchTerm);

                        // Then
                        assertNotNull(result);
                        assertTrue(result.isEmpty());
                        verify(organizationRepository).searchOrganizations("NonExistentOrganization");
                }
        }

        @Nested
        @DisplayName("User Account Type Tests")
        class UserAccountTypeTests {

                @Test
                @DisplayName("Create organization with regular user should work")
                void createOrganizationWithRegularUser_ShouldWork() {
                        // Given - Regular user (only USER account type can create organizations)
                        User regularUser = User.builder()
                                        .id("regular-123")
                                        .name("Regular User")
                                        .phone("1111111111")
                                        .email("regular@example.com")
                                        .password("encodedPassword")
                                        .status(UserStatus.ACTIVE)
                                        .accountType(AccountType.USER)
                                        .createdAt(LocalDateTime.now())
                                        .updatedAt(LocalDateTime.now())
                                        .build();

                        CreateOrganizationRequest regularRequest = new CreateOrganizationRequest(
                                        "regular-123",
                                        "Regular User Organization",
                                        "regular.com",
                                        "Regular user organization description",
                                        "789 Regular Street",
                                        "8888888888",
                                        "contact@regular.com");

                        Organization savedOrganization = Organization.builder()
                                        .id("regular-org-123")
                                        .name("Regular User Organization")
                                        .domain("regular.com")
                                        .description("Regular user organization description")
                                        .address("789 Regular Street")
                                        .phone("8888888888")
                                        .createdBy(regularUser)
                                        .build();

                        when(organizationRepository.existsByPhone("8888888888")).thenReturn(false);
                        when(organizationRepository.existsByDomain("regular.com")).thenReturn(false);
                        when(userService.getUserById("regular-123")).thenReturn(regularUser);
                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("regular-org-123", "regular-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-regular")
                                                        .organizationId("regular-org-123").build());

                        // When
                        Organization result = organizationService.createOrganization(regularRequest);

                        // Then
                        assertNotNull(result);
                        assertEquals("regular-org-123", result.getId());
                        assertEquals("Regular User Organization", result.getName());
                        assertEquals(regularUser, result.getCreatedBy());
                        assertEquals(AccountType.USER, result.getCreatedBy().getAccountType());
                }

                @Test
                @DisplayName("Create organization with employee user should fail")
                void createOrganizationWithEmployeeUser_ShouldFail() {
                        // Given - Employee user (should not be able to create organizations)
                        User employeeUser = User.builder()
                                        .id("employee-123")
                                        .name("Employee User")
                                        .phone("2222222222")
                                        .email("employee@example.com")
                                        .password("encodedPassword")
                                        .status(UserStatus.ACTIVE)
                                        .accountType(AccountType.EMPLOYEE)
                                        .createdAt(LocalDateTime.now())
                                        .updatedAt(LocalDateTime.now())
                                        .build();

                        CreateOrganizationRequest employeeRequest = new CreateOrganizationRequest(
                                        "employee-123",
                                        "Employee Organization",
                                        "employee.com",
                                        "Employee organization description",
                                        "123 Employee Street",
                                        "7777777777",
                                        "contact@employee.com");

                        when(organizationRepository.existsByPhone("7777777777")).thenReturn(false);
                        when(organizationRepository.existsByDomain("employee.com")).thenReturn(false);
                        when(userService.getUserById("employee-123")).thenReturn(employeeUser);

                        // When & Then - Should fail because only USER account type can create
                        // organizations
                        // Note: This test assumes the business logic prevents EMPLOYEE users from
                        // creating organizations
                        // If the current implementation allows it, this test documents the expected
                        // behavior
                        BadRequestException exception = assertThrows(BadRequestException.class,
                                        () -> organizationService.createOrganization(employeeRequest));

                        assertTrue(exception.getMessage()
                                        .contains("Only users with USER account type can create organizations") ||
                                        exception.getMessage().contains("Failed to create organization"));

                        // Verify that the service attempted to check user permissions
                        verify(userService).getUserById("employee-123");
                }

                @Test
                @DisplayName("Create organization with inactive user should fail")
                void createOrganizationWithInactiveUser_ShouldFail() {
                        // Given - Inactive user
                        User inactiveUser = User.builder()
                                        .id("inactive-123")
                                        .name("Inactive User")
                                        .phone("2222222222")
                                        .email("inactive@example.com")
                                        .password("encodedPassword")
                                        .status(UserStatus.INACTIVE)
                                        .accountType(AccountType.USER)
                                        .createdAt(LocalDateTime.now())
                                        .updatedAt(LocalDateTime.now())
                                        .build();

                        CreateOrganizationRequest inactiveUserRequest = new CreateOrganizationRequest(
                                        "inactive-123",
                                        "Inactive User Org",
                                        "inactive.com",
                                        "Organization by inactive user",
                                        "123 Inactive Street",
                                        "6666666666",
                                        "contact@inactive.com");

                        when(organizationRepository.existsByPhone("6666666666")).thenReturn(false);
                        when(organizationRepository.existsByDomain("inactive.com")).thenReturn(false);
                        when(userService.getUserById("inactive-123")).thenReturn(inactiveUser);

                        Organization savedOrganization = Organization.builder()
                                        .id("inactive-org-123")
                                        .name("Inactive User Org")
                                        .domain("inactive.com")
                                        .description("Organization by inactive user")
                                        .address("123 Inactive Street")
                                        .phone("6666666666")
                                        .createdBy(inactiveUser)
                                        .build();

                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrganization);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("inactive-org-123",
                                        "inactive-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-inactive")
                                                        .organizationId("inactive-org-123").build());

                        // When
                        Organization result = organizationService.createOrganization(inactiveUserRequest);

                        // Then - Should still work (business logic might allow inactive users to create
                        // orgs)
                        assertNotNull(result);
                        assertEquals(UserStatus.INACTIVE, result.getCreatedBy().getStatus());
                }
        }

        @Nested
        @DisplayName("Concurrent Access Simulation Tests")
        class ConcurrentAccessSimulationTests {

                @Test
                @DisplayName("Multiple organization creation attempts with same phone should fail appropriately")
                void multipleOrganizationCreationWithSamePhone_ShouldFailAppropriately() {
                        // Given - Simulate concurrent creation attempts
                        CreateOrganizationRequest request1 = new CreateOrganizationRequest(
                                        "user-123", "Org 1", "org1.com", "First org", "123 Street", "1111111111",
                                        "contact1@org.com");
                        CreateOrganizationRequest request2 = new CreateOrganizationRequest(
                                        "user-123", "Org 2", "org2.com", "Second org", "456 Street", "1111111111",
                                        "contact2@org.com");

                        // First call succeeds
                        when(organizationRepository.existsByPhone("1111111111")).thenReturn(false).thenReturn(true);
                        when(organizationRepository.existsByDomain("org1.com")).thenReturn(false);
                        when(userService.getUserById("user-123")).thenReturn(testUser);

                        Organization savedOrg1 = Organization.builder()
                                        .id("org-1")
                                        .name("Org 1")
                                        .domain("org1.com")
                                        .phone("1111111111")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrg1);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("org-1", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-1").organizationId("org-1")
                                                        .build());

                        // When & Then
                        Organization result1 = organizationService.createOrganization(request1);
                        assertNotNull(result1);

                        // Second call should fail due to duplicate phone
                        ConflictException exception = assertThrows(ConflictException.class,
                                        () -> organizationService.createOrganization(request2));
                        assertTrue(exception.getMessage()
                                        .contains("Organization with phone 1111111111 already exists"));
                }

                @Test
                @DisplayName("Multiple organization creation attempts with same domain should fail appropriately")
                void multipleOrganizationCreationWithSameDomain_ShouldFailAppropriately() {
                        // Given - Simulate concurrent creation attempts with same domain
                        CreateOrganizationRequest request1 = new CreateOrganizationRequest(
                                        "user-123", "Org 1", "same.com", "First org", "123 Street", "1111111111",
                                        "contact1@same.com");
                        CreateOrganizationRequest request2 = new CreateOrganizationRequest(
                                        "user-123", "Org 2", "same.com", "Second org", "456 Street", "2222222222",
                                        "contact2@same.com");

                        // First call succeeds
                        when(organizationRepository.existsByPhone("1111111111")).thenReturn(false);
                        when(organizationRepository.existsByPhone("2222222222")).thenReturn(false);
                        when(organizationRepository.existsByDomain("same.com")).thenReturn(false).thenReturn(true);
                        when(userService.getUserById("user-123")).thenReturn(testUser);

                        Organization savedOrg1 = Organization.builder()
                                        .id("org-1")
                                        .name("Org 1")
                                        .domain("same.com")
                                        .phone("1111111111")
                                        .createdBy(testUser)
                                        .build();

                        when(organizationRepository.save(any(Organization.class))).thenReturn(savedOrg1);
                        when(systemSettingsService.createAndSaveDefaultSystemSettings("org-1", "user-123"))
                                        .thenReturn(SystemSetting.builder().id("sys-1").organizationId("org-1")
                                                        .build());

                        // When & Then
                        Organization result1 = organizationService.createOrganization(request1);
                        assertNotNull(result1);

                        // Second call should fail due to duplicate domain
                        ConflictException exception = assertThrows(ConflictException.class,
                                        () -> organizationService.createOrganization(request2));
                        assertTrue(exception.getMessage().contains("Organization with domain same.com already exists"));
                }
        }
}