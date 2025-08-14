package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.config.utils.JwtUtil;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserRepository;
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
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Integration tests for AuthService that test the complete flow
 * including service interactions and business logic validation.
 */
@SpringBootTest
@ActiveProfiles("test")
@Transactional
@DirtiesContext
class AuthServiceIntegrationTest {

    @Autowired
    private AuthService authService;

    @MockitoBean
    private UserRepository userRepository;

    @MockitoBean
    private UserService userService;

    @MockitoBean
    private JwtUtil jwtUtil;

    private BCryptPasswordEncoder passwordEncoder;
    private User testUser;
    private String rawPassword;
    private String encodedPassword;
    private String jwtToken;

    @BeforeEach
    void setUp() {
        passwordEncoder = new BCryptPasswordEncoder();
        rawPassword = "password123";
        encodedPassword = passwordEncoder.encode(rawPassword);
        jwtToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token";

        testUser = User.builder()
                .id("user-123")
                .name("Test User")
                .phone("1234567890")
                .email("test@example.com")
                .password(encodedPassword)
                .status(UserStatus.ACTIVE)
                .accountType(AccountType.USER)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    @Nested
    @DisplayName("Login Integration Tests")
    class LoginIntegrationTests {

        @Test
        @DisplayName("Complete login flow with email should work end-to-end")
        void completeLoginFlowWithEmail_ShouldWorkEndToEnd() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", rawPassword);

            when(userRepository.findByEmail("test@example.com")).thenReturn(Optional.of(testUser));
            when(userRepository.findByPhone("test@example.com")).thenReturn(Optional.empty());
            when(userRepository.save(any(User.class))).thenReturn(testUser);
            when(jwtUtil.generateToken(testUser.getId(), testUser.getPhone(), testUser.getName()))
                    .thenReturn(jwtToken);

            // When
            LoginResponse response = authService.login(loginRequest);

            // Then
            assertNotNull(response);
            assertEquals(jwtToken, response.getToken());
            assertEquals("user-123", response.getUserId());
            assertEquals("Test User", response.getName());
            assertEquals("1234567890", response.getPhone());
            assertEquals("Login successful", response.getMessage());

            // Verify interactions
            verify(userRepository).findByEmail("test@example.com");
            verify(userRepository).save(argThat(user -> user.getLastLoginAt() != null &&
                    user.getId().equals("user-123")));
            verify(jwtUtil).generateToken(testUser.getId(), testUser.getPhone(), testUser.getName());
        }

        @Test
        @DisplayName("Complete login flow with phone should work end-to-end")
        void completeLoginFlowWithPhone_ShouldWorkEndToEnd() {
            // Given
            LoginRequest loginRequest = new LoginRequest("1234567890", rawPassword);

            when(userRepository.findByEmail("1234567890")).thenReturn(Optional.empty());
            when(userRepository.findByPhone("1234567890")).thenReturn(Optional.of(testUser));
            when(userRepository.save(any(User.class))).thenReturn(testUser);
            when(jwtUtil.generateToken(testUser.getId(), testUser.getPhone(), testUser.getName()))
                    .thenReturn(jwtToken);

            // When
            LoginResponse response = authService.login(loginRequest);

            // Then
            assertNotNull(response);
            assertEquals(jwtToken, response.getToken());
            assertEquals("user-123", response.getUserId());
            assertEquals("Test User", response.getName());
            assertEquals("1234567890", response.getPhone());
            assertEquals("Login successful", response.getMessage());

            // Verify fallback to phone lookup
            verify(userRepository).findByEmail("1234567890");
            verify(userRepository).findByPhone("1234567890");
            verify(userRepository).save(any(User.class));
        }

        @Test
        @DisplayName("Login with wrong password should fail with proper error")
        void loginWithWrongPassword_ShouldFailWithProperError() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "wrongpassword");

            when(userRepository.findByEmail("test@example.com")).thenReturn(Optional.of(testUser));
            when(userRepository.findByPhone("test@example.com")).thenReturn(Optional.empty());

            // When & Then
            UnauthorizedException exception = assertThrows(UnauthorizedException.class,
                    () -> authService.login(loginRequest));

            assertEquals("Invalid credentials", exception.getMessage());

            // Verify no token generation or user update
            verify(userRepository, never()).save(any(User.class));
            verify(jwtUtil, never()).generateToken(anyString(), anyString(), anyString());
        }

        @Test
        @DisplayName("Login with inactive user should fail")
        void loginWithInactiveUser_ShouldFail() {
            // Given
            testUser.setStatus(UserStatus.INACTIVE);
            LoginRequest loginRequest = new LoginRequest("test@example.com", rawPassword);

            when(userRepository.findByEmail("test@example.com")).thenReturn(Optional.of(testUser));
            when(userRepository.findByPhone("test@example.com")).thenReturn(Optional.empty());

            // When & Then
            UnauthorizedException exception = assertThrows(UnauthorizedException.class,
                    () -> authService.login(loginRequest));

            assertEquals("Account is not active", exception.getMessage());

            // Verify no token generation or user update
            verify(userRepository, never()).save(any(User.class));
            verify(jwtUtil, never()).generateToken(anyString(), anyString(), anyString());
        }
    }

    @Nested
    @DisplayName("Registration Integration Tests")
    class RegistrationIntegrationTests {

        @Test
        @DisplayName("Complete registration flow should work end-to-end")
        void completeRegistrationFlow_ShouldWorkEndToEnd() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "New User",
                    "newuser@example.com",
                    "9876543210",
                    "newpassword123");

            User savedUser = User.builder()
                    .id("new-user-456")
                    .name("New User")
                    .phone("9876543210")
                    .email("newuser@example.com")
                    .password(passwordEncoder.encode("newpassword123"))
                    .status(UserStatus.ACTIVE)
                    .accountType(AccountType.USER)
                    .build();

            when(userRepository.existsByPhone("9876543210")).thenReturn(false);
            when(userRepository.existsByEmail("newuser@example.com")).thenReturn(false);
            when(userService.createUser(any(User.class))).thenReturn(savedUser);
            when(jwtUtil.generateToken(savedUser.getId(), savedUser.getPhone(), savedUser.getName()))
                    .thenReturn(jwtToken);

            // When
            LoginResponse response = authService.register(registerRequest);

            // Then
            assertNotNull(response);
            assertEquals(jwtToken, response.getToken());
            assertEquals("new-user-456", response.getUserId());
            assertEquals("New User", response.getName());
            assertEquals("9876543210", response.getPhone());
            assertEquals("Registration successful", response.getMessage());

            // Verify the complete flow
            verify(userRepository).existsByPhone("9876543210");
            verify(userRepository).existsByEmail("newuser@example.com");
            verify(userService).createUser(argThat(user -> user.getName().equals("New User") &&
                    user.getPhone().equals("9876543210") &&
                    user.getEmail().equals("newuser@example.com") &&
                    user.getStatus() == UserStatus.ACTIVE &&
                    user.getAccountType() == AccountType.USER &&
                    passwordEncoder.matches("newpassword123", user.getPassword())));
            verify(jwtUtil).generateToken(savedUser.getId(), savedUser.getPhone(), savedUser.getName());
        }

        @Test
        @DisplayName("Registration with existing phone should fail")
        void registrationWithExistingPhone_ShouldFail() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "New User",
                    "newuser@example.com",
                    "1234567890", // existing phone
                    "newpassword123");

            when(userRepository.existsByPhone("1234567890")).thenReturn(true);

            // When & Then
            ConflictException exception = assertThrows(ConflictException.class,
                    () -> authService.register(registerRequest));

            assertEquals("Phone number already exists or is empty: 1234567890", exception.getMessage());

            // Verify no user creation
            verify(userRepository).existsByPhone("1234567890");
            verify(userRepository, never()).existsByEmail(anyString());
            verify(userService, never()).createUser(any(User.class));
            verify(jwtUtil, never()).generateToken(anyString(), anyString(), anyString());
        }

        @Test
        @DisplayName("Registration with existing email should fail")
        void registrationWithExistingEmail_ShouldFail() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "New User",
                    "test@example.com", // existing email
                    "9876543210",
                    "newpassword123");

            when(userRepository.existsByPhone("9876543210")).thenReturn(false);
            when(userRepository.existsByEmail("test@example.com")).thenReturn(true);

            // When & Then
            ConflictException exception = assertThrows(ConflictException.class,
                    () -> authService.register(registerRequest));

            assertEquals("Email already exists: test@example.com", exception.getMessage());

            // Verify partial flow execution
            verify(userRepository).existsByPhone("9876543210");
            verify(userRepository).existsByEmail("test@example.com");
            verify(userService, never()).createUser(any(User.class));
            verify(jwtUtil, never()).generateToken(anyString(), anyString(), anyString());
        }

        @Test
        @DisplayName("Registration with empty phone should fail")
        void registrationWithEmptyPhone_ShouldFail() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "New User",
                    "newuser@example.com",
                    "", // empty phone
                    "newpassword123");

            // When & Then
            ConflictException exception = assertThrows(ConflictException.class,
                    () -> authService.register(registerRequest));

            assertEquals("Phone number already exists or is empty: ", exception.getMessage());

            // Verify no further processing
            verify(userRepository).existsByPhone("");
            verify(userRepository, never()).existsByEmail(anyString());
            verify(userService, never()).createUser(any(User.class));
        }
    }

    @Nested
    @DisplayName("User Existence Check Integration Tests")
    class UserExistenceCheckIntegrationTests {

        @Test
        @DisplayName("User existence check by email should work correctly")
        void userExistenceCheckByEmail_ShouldWorkCorrectly() {
            // Given
            when(userRepository.findByEmail("test@example.com")).thenReturn(Optional.of(testUser));

            // When
            boolean exists = authService.userExists("test@example.com");

            // Then
            assertTrue(exists);
            verify(userRepository).findByEmail("test@example.com");
            verify(userRepository, never()).findByPhone(anyString());
        }

        @Test
        @DisplayName("User existence check by phone should work correctly")
        void userExistenceCheckByPhone_ShouldWorkCorrectly() {
            // Given
            when(userRepository.findByEmail("1234567890")).thenReturn(Optional.empty());
            when(userRepository.findByPhone("1234567890")).thenReturn(Optional.of(testUser));

            // When
            boolean exists = authService.userExists("1234567890");

            // Then
            assertTrue(exists);
            verify(userRepository).findByEmail("1234567890");
            verify(userRepository).findByPhone("1234567890");
        }

        @Test
        @DisplayName("Non-existent user check should return false")
        void nonExistentUserCheck_ShouldReturnFalse() {
            // Given
            when(userRepository.findByEmail("nonexistent")).thenReturn(Optional.empty());
            when(userRepository.findByPhone("nonexistent")).thenReturn(Optional.empty());

            // When
            boolean exists = authService.userExists("nonexistent");

            // Then
            assertFalse(exists);
            verify(userRepository).findByEmail("nonexistent");
            verify(userRepository).findByPhone("nonexistent");
        }

        @Test
        @DisplayName("Phone existence check should work correctly")
        void phoneExistenceCheck_ShouldWorkCorrectly() {
            // Given
            when(userRepository.existsByPhone("1234567890")).thenReturn(true);
            when(userRepository.existsByPhone("9999999999")).thenReturn(false);

            // When & Then
            assertTrue(authService.phoneExists("1234567890"));
            assertFalse(authService.phoneExists("9999999999"));

            verify(userRepository).existsByPhone("1234567890");
            verify(userRepository).existsByPhone("9999999999");
        }

        @Test
        @DisplayName("Email existence check should work correctly")
        void emailExistenceCheck_ShouldWorkCorrectly() {
            // Given
            when(userRepository.existsByEmail("test@example.com")).thenReturn(true);
            when(userRepository.existsByEmail("nonexistent@example.com")).thenReturn(false);

            // When & Then
            assertTrue(authService.emailExists("test@example.com"));
            assertFalse(authService.emailExists("nonexistent@example.com"));

            verify(userRepository).existsByEmail("test@example.com");
            verify(userRepository).existsByEmail("nonexistent@example.com");
        }
    }

    @Nested
    @DisplayName("Password Change Integration Tests")
    class PasswordChangeIntegrationTests {

        @Test
        @DisplayName("Password change should work end-to-end")
        void passwordChange_ShouldWorkEndToEnd() {
            // Given
            when(userService.getUserById("user-123")).thenReturn(testUser);
            when(userService.updateUser(eq("user-123"), any(User.class))).thenReturn(testUser);

            // When
            boolean result = authService.changePassword("user-123", rawPassword, "newpassword123");

            // Then
            assertTrue(result);
            verify(userService).getUserById("user-123");
            verify(userService).updateUser(eq("user-123"),
                    argThat(user -> passwordEncoder.matches("newpassword123", user.getPassword())));
        }

        @Test
        @DisplayName("Password change with wrong old password should fail")
        void passwordChangeWithWrongOldPassword_ShouldFail() {
            // Given
            when(userService.getUserById("user-123")).thenReturn(testUser);

            // When & Then
            UnauthorizedException exception = assertThrows(UnauthorizedException.class,
                    () -> authService.changePassword("user-123", "wrongoldpassword", "newpassword123"));

            assertEquals("Old password is incorrect", exception.getMessage());
            verify(userService).getUserById("user-123");
            verify(userService, never()).updateUser(anyString(), any(User.class));
        }

        @Test
        @DisplayName("Password change for non-existent user should fail")
        void passwordChangeForNonExistentUser_ShouldFail() {
            // Given
            when(userService.getUserById("nonexistent")).thenReturn(null);

            // When & Then
            NotFoundException exception = assertThrows(NotFoundException.class,
                    () -> authService.changePassword("nonexistent", "oldpassword", "newpassword"));

            assertEquals("User not found", exception.getMessage());
            verify(userService).getUserById("nonexistent");
            verify(userService, never()).updateUser(anyString(), any(User.class));
        }
    }

    @Nested
    @DisplayName("Token Validation Integration Tests")
    class TokenValidationIntegrationTests {

        @Test
        @DisplayName("Valid token validation should work")
        void validTokenValidation_ShouldWork() {
            // Given
            when(jwtUtil.validateToken("valid.token")).thenReturn(true);

            // When
            boolean isValid = authService.validateToken("valid.token");

            // Then
            assertTrue(isValid);
            verify(jwtUtil).validateToken("valid.token");
        }

        @Test
        @DisplayName("Invalid token validation should return false")
        void invalidTokenValidation_ShouldReturnFalse() {
            // Given
            when(jwtUtil.validateToken("invalid.token")).thenReturn(false);

            // When
            boolean isValid = authService.validateToken("invalid.token");

            // Then
            assertFalse(isValid);
            verify(jwtUtil).validateToken("invalid.token");
        }

        @Test
        @DisplayName("Token validation with exception should return false")
        void tokenValidationWithException_ShouldReturnFalse() {
            // Given
            when(jwtUtil.validateToken("malformed.token")).thenThrow(new RuntimeException("Token malformed"));

            // When
            boolean isValid = authService.validateToken("malformed.token");

            // Then
            assertFalse(isValid);
            verify(jwtUtil).validateToken("malformed.token");
        }
    }
}