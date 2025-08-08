package com.sajjadkademm.retail.auth.dto;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comprehensive tests for Auth DTOs including validation constraints
 */
class AuthDtoTest {

    private Validator validator;

    @BeforeEach
    void setUp() {
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        validator = factory.getValidator();
    }

    @Nested
    @DisplayName("LoginRequest Tests")
    class LoginRequestTests {

        @Test
        @DisplayName("Valid LoginRequest should pass validation")
        void validLoginRequest_ShouldPassValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "password123");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("LoginRequest with phone should pass validation")
        void loginRequestWithPhone_ShouldPassValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest("1234567890", "password123");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("LoginRequest with blank emailOrPhone should fail validation")
        void loginRequestWithBlankEmailOrPhone_ShouldFailValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest("", "password123");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<LoginRequest> violation = violations.iterator().next();
            assertEquals("Email or phone is required", violation.getMessage());
            assertEquals("emailOrPhone", violation.getPropertyPath().toString());
        }

        @Test
        @DisplayName("LoginRequest with null emailOrPhone should fail validation")
        void loginRequestWithNullEmailOrPhone_ShouldFailValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest(null, "password123");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<LoginRequest> violation = violations.iterator().next();
            assertEquals("Email or phone is required", violation.getMessage());
        }

        @Test
        @DisplayName("LoginRequest with blank password should fail validation")
        void loginRequestWithBlankPassword_ShouldFailValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<LoginRequest> violation = violations.iterator().next();
            assertEquals("Password is required", violation.getMessage());
            assertEquals("password", violation.getPropertyPath().toString());
        }

        @Test
        @DisplayName("LoginRequest with short password should fail validation")
        void loginRequestWithShortPassword_ShouldFailValidation() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "short");

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<LoginRequest> violation = violations.iterator().next();
            assertEquals("Password must be between 8 and 32 characters", violation.getMessage());
        }

        @Test
        @DisplayName("LoginRequest with long password should fail validation")
        void loginRequestWithLongPassword_ShouldFailValidation() {
            // Given
            String longPassword = "a".repeat(33); // 33 characters
            LoginRequest loginRequest = new LoginRequest("test@example.com", longPassword);

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<LoginRequest> violation = violations.iterator().next();
            assertEquals("Password must be between 8 and 32 characters", violation.getMessage());
        }

        @Test
        @DisplayName("LoginRequest with minimum valid password length should pass")
        void loginRequestWithMinimumPasswordLength_ShouldPass() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "12345678"); // 8 characters

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("LoginRequest with maximum valid password length should pass")
        void loginRequestWithMaximumPasswordLength_ShouldPass() {
            // Given
            String maxPassword = "a".repeat(32); // 32 characters
            LoginRequest loginRequest = new LoginRequest("test@example.com", maxPassword);

            // When
            Set<ConstraintViolation<LoginRequest>> violations = validator.validate(loginRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("LoginRequest equality and hashCode should work correctly")
        void loginRequestEqualityAndHashCode_ShouldWorkCorrectly() {
            // Given
            LoginRequest request1 = new LoginRequest("test@example.com", "password123");
            LoginRequest request2 = new LoginRequest("test@example.com", "password123");
            LoginRequest request3 = new LoginRequest("different@example.com", "password123");

            // Then
            assertEquals(request1, request2);
            assertEquals(request1.hashCode(), request2.hashCode());
            assertNotEquals(request1, request3);
            assertNotEquals(request1.hashCode(), request3.hashCode());
        }

        @Test
        @DisplayName("LoginRequest toString should work correctly")
        void loginRequestToString_ShouldWorkCorrectly() {
            // Given
            LoginRequest loginRequest = new LoginRequest("test@example.com", "password123");

            // When
            String toString = loginRequest.toString();

            // Then
            assertNotNull(toString);
            assertTrue(toString.contains("test@example.com"));
            assertTrue(toString.contains("password123"));
        }
    }

    @Nested
    @DisplayName("RegisterRequest Tests")
    class RegisterRequestTests {

        @Test
        @DisplayName("Valid RegisterRequest should pass validation")
        void validRegisterRequest_ShouldPassValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "1234567890",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("RegisterRequest with blank name should fail validation")
        void registerRequestWithBlankName_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "",
                    "john@example.com",
                    "1234567890",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Name is required", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with short name should fail validation")
        void registerRequestWithShortName_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "Jo", // 2 characters
                    "john@example.com",
                    "1234567890",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Name must be between 3 and 255 characters", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with long name should fail validation")
        void registerRequestWithLongName_ShouldFailValidation() {
            // Given
            String longName = "a".repeat(256); // 256 characters
            RegisterRequest registerRequest = new RegisterRequest(
                    longName,
                    "john@example.com",
                    "1234567890",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Name must be between 3 and 255 characters", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with invalid email should fail validation")
        void registerRequestWithInvalidEmail_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "invalid-email",
                    "1234567890",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Invalid email address", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with blank phone should fail validation")
        void registerRequestWithBlankPhone_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "",
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Phone is required", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with short phone should fail validation")
        void registerRequestWithShortPhone_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "123456789", // 9 characters
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Phone must be between 10 and 20 digits", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with long phone should fail validation")
        void registerRequestWithLongPhone_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "123456789012345678901", // 21 characters
                    "password123"
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Phone must be between 10 and 20 digits", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with blank password should fail validation")
        void registerRequestWithBlankPassword_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "1234567890",
                    ""
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Password is required", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with short password should fail validation")
        void registerRequestWithShortPassword_ShouldFailValidation() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "John Doe",
                    "john@example.com",
                    "1234567890",
                    "short" // 5 characters
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertEquals(1, violations.size());
            ConstraintViolation<RegisterRequest> violation = violations.iterator().next();
            assertEquals("Password must be between 8 and 32 characters", violation.getMessage());
        }

        @Test
        @DisplayName("RegisterRequest with minimum valid lengths should pass")
        void registerRequestWithMinimumValidLengths_ShouldPass() {
            // Given
            RegisterRequest registerRequest = new RegisterRequest(
                    "Jon", // 3 characters (minimum)
                    "a@b.co", // valid email
                    "1234567890", // 10 characters (minimum)
                    "12345678" // 8 characters (minimum)
            );

            // When
            Set<ConstraintViolation<RegisterRequest>> violations = validator.validate(registerRequest);

            // Then
            assertTrue(violations.isEmpty());
        }

        @Test
        @DisplayName("RegisterRequest equality and hashCode should work correctly")
        void registerRequestEqualityAndHashCode_ShouldWorkCorrectly() {
            // Given
            RegisterRequest request1 = new RegisterRequest("John Doe", "john@example.com", "1234567890", "password123");
            RegisterRequest request2 = new RegisterRequest("John Doe", "john@example.com", "1234567890", "password123");
            RegisterRequest request3 = new RegisterRequest("Jane Doe", "jane@example.com", "0987654321", "password456");

            // Then
            assertEquals(request1, request2);
            assertEquals(request1.hashCode(), request2.hashCode());
            assertNotEquals(request1, request3);
            assertNotEquals(request1.hashCode(), request3.hashCode());
        }
    }

    @Nested
    @DisplayName("LoginResponse Tests")
    class LoginResponseTests {

        @Test
        @DisplayName("LoginResponse builder should work correctly")
        void loginResponseBuilder_ShouldWorkCorrectly() {
            // Given & When
            LoginResponse response = LoginResponse.builder()
                    .token("jwt.token.here")
                    .userId("user-123")
                    .name("John Doe")
                    .email("john@example.com")
                    .phone("1234567890")
                    .message("Login successful")
                    .build();

            // Then
            assertEquals("jwt.token.here", response.getToken());
            assertEquals("user-123", response.getUserId());
            assertEquals("John Doe", response.getName());
            assertEquals("john@example.com", response.getEmail());
            assertEquals("1234567890", response.getPhone());
            assertEquals("Login successful", response.getMessage());
        }

        @Test
        @DisplayName("LoginResponse no-args constructor should work")
        void loginResponseNoArgsConstructor_ShouldWork() {
            // Given & When
            LoginResponse response = new LoginResponse();

            // Then
            assertNull(response.getToken());
            assertNull(response.getUserId());
            assertNull(response.getName());
            assertNull(response.getEmail());
            assertNull(response.getPhone());
            assertNull(response.getMessage());
        }

        @Test
        @DisplayName("LoginResponse all-args constructor should work")
        void loginResponseAllArgsConstructor_ShouldWork() {
            // Given & When
            LoginResponse response = new LoginResponse(
                    "jwt.token.here",
                    "user-123",
                    "John Doe",
                    "john@example.com",
                    "1234567890",
                    "Login successful"
            );

            // Then
            assertEquals("jwt.token.here", response.getToken());
            assertEquals("user-123", response.getUserId());
            assertEquals("John Doe", response.getName());
            assertEquals("john@example.com", response.getEmail());
            assertEquals("1234567890", response.getPhone());
            assertEquals("Login successful", response.getMessage());
        }

        @Test
        @DisplayName("LoginResponse setters should work correctly")
        void loginResponseSetters_ShouldWorkCorrectly() {
            // Given
            LoginResponse response = new LoginResponse();

            // When
            response.setToken("new.token");
            response.setUserId("new-user-456");
            response.setName("Jane Doe");
            response.setEmail("jane@example.com");
            response.setPhone("0987654321");
            response.setMessage("Registration successful");

            // Then
            assertEquals("new.token", response.getToken());
            assertEquals("new-user-456", response.getUserId());
            assertEquals("Jane Doe", response.getName());
            assertEquals("jane@example.com", response.getEmail());
            assertEquals("0987654321", response.getPhone());
            assertEquals("Registration successful", response.getMessage());
        }

        @Test
        @DisplayName("LoginResponse equality and hashCode should work correctly")
        void loginResponseEqualityAndHashCode_ShouldWorkCorrectly() {
            // Given
            LoginResponse response1 = LoginResponse.builder()
                    .token("token")
                    .userId("user-123")
                    .name("John Doe")
                    .email("john@example.com")
                    .phone("1234567890")
                    .message("Success")
                    .build();

            LoginResponse response2 = LoginResponse.builder()
                    .token("token")
                    .userId("user-123")
                    .name("John Doe")
                    .email("john@example.com")
                    .phone("1234567890")
                    .message("Success")
                    .build();

            LoginResponse response3 = LoginResponse.builder()
                    .token("different-token")
                    .userId("user-456")
                    .name("Jane Doe")
                    .email("jane@example.com")
                    .phone("0987654321")
                    .message("Different")
                    .build();

            // Then
            assertEquals(response1, response2);
            assertEquals(response1.hashCode(), response2.hashCode());
            assertNotEquals(response1, response3);
            assertNotEquals(response1.hashCode(), response3.hashCode());
        }

        @Test
        @DisplayName("LoginResponse toString should work correctly")
        void loginResponseToString_ShouldWorkCorrectly() {
            // Given
            LoginResponse response = LoginResponse.builder()
                    .token("jwt.token.here")
                    .userId("user-123")
                    .name("John Doe")
                    .email("john@example.com")
                    .phone("1234567890")
                    .message("Login successful")
                    .build();

            // When
            String toString = response.toString();

            // Then
            assertNotNull(toString);
            assertTrue(toString.contains("user-123"));
            assertTrue(toString.contains("John Doe"));
            assertTrue(toString.contains("john@example.com"));
            assertTrue(toString.contains("1234567890"));
            assertTrue(toString.contains("Login successful"));
        }
    }
}