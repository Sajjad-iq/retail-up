package com.sajjadkademm.retail.application.controllers.auth;

import com.sajjadkademm.retail.application.services.auth.AuthService;
import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.application.dto.auth.ChangePasswordRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.application.dto.auth.RegisterRequest;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.application.services.users.UserService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

/**
 * Authentication controller providing user authentication and authorization
 * endpoints.
 * All operations are scoped to the current authenticated user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Authentication", description = "Authentication and authorization endpoints")
public class AuthController {

        private final AuthService authService;
        private final UserService userService;

        /**
         * User login endpoint
         */
        @Operation(summary = "User Login", description = "Authenticate a user with email/phone and password")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Login successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "401", description = "Authentication failed"),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @PostMapping("/login")
        public LoginResponse login(
                        @Parameter(description = "Login credentials", required = true) @Valid @RequestBody LoginRequest request) {
                return authService.login(request);
        }

        /**
         * User registration endpoint
         */
        @Operation(summary = "User Registration", description = "Register a new user account")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Registration successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "409", description = "User already exists"),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @PostMapping("/register")
        public LoginResponse register(
                        @Parameter(description = "Registration information", required = true) @Valid @RequestBody RegisterRequest request) {
                return authService.register(request);
        }

        /**
         * Get current user profile endpoint
         */
        @Operation(summary = "Get Current User Profile", description = "Retrieve the current authenticated user's profile", security = @SecurityRequirement(name = "Bearer Authentication"))
        @ApiResponse(responseCode = "200", description = "User profile retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = User.class)))
        @GetMapping("/me")
        public User getCurrentUserProfile() {
                return userService.getCurrentUserProfile();
        }

        /**
         * Check if user exists
         */
        @Operation(summary = "Check User Exists", description = "Check if a user exists by email or phone number")
        @ApiResponse(responseCode = "200", description = "User existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean")))
        @GetMapping("/exists")
        public boolean userExists(
                        @Parameter(description = "Email or phone number to check", required = true) @RequestParam String emailOrPhone) {
                return authService.userExists(emailOrPhone);
        }

        /**
         * Change current user password endpoint
         */
        @Operation(summary = "Change Current User Password", description = "Change the current authenticated user's password", security = @SecurityRequirement(name = "Bearer Authentication"))
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request"),
                        @ApiResponse(responseCode = "401", description = "Unauthorized")
        })
        @PostMapping("/change-password")
        public AuthResponse changePassword(
                        @Parameter(description = "Password change request", required = true) @Valid @RequestBody ChangePasswordRequest request) {
                return authService.changePasswordWithResponse(request.getOldPassword(),
                                request.getNewPassword());
        }

        /**
         * Validate JWT token endpoint
         */
        @Operation(summary = "Validate JWT Token", description = "Validate if a JWT token is valid and return user information")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Token is valid", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @GetMapping("/validate-token")
        public LoginResponse validateToken() {
                return authService.refreshToken();
        }
}