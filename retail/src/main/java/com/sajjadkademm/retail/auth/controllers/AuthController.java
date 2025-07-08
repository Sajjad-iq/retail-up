package com.sajjadkademm.retail.auth.controllers;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.services.AuthService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Authentication controller for handling login, logout, and session management.
 * Provides endpoints for user authentication operations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Authentication", description = "Authentication management endpoints")
public class AuthController {

    private final AuthService authService;

    /**
     * User login endpoint
     */
    @PostMapping("/login")
    @Operation(summary = "User login", description = "Authenticate user and create session")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Login successful"),
            @ApiResponse(responseCode = "400", description = "Invalid credentials"),
            @ApiResponse(responseCode = "401", description = "Authentication failed")
    })
    public ResponseEntity<LoginResponse> login(@Valid @RequestBody LoginRequest loginRequest,
            HttpServletRequest request,
            HttpServletResponse response) {
        log.info("Login request received for email: {}", loginRequest.getEmail());

        LoginResponse loginResponse = authService.login(loginRequest, request, response);

        log.info("Login successful for user: {}", loginRequest.getEmail());
        return ResponseEntity.ok(loginResponse);
    }

    /**
     * User logout endpoint
     */
    @PostMapping("/logout")
    @Operation(summary = "User logout", description = "Logout user and invalidate session")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Logout successful"),
            @ApiResponse(responseCode = "401", description = "Unauthorized")
    })
    public ResponseEntity<Map<String, String>> logout(HttpServletRequest request,
            HttpServletResponse response) {
        log.info("Logout request received");

        authService.logout(request, response);

        return ResponseEntity.ok(Map.of(
                "message", "Logout successful",
                "timestamp", java.time.LocalDateTime.now().toString()));
    }

    /**
     * Logout from all devices
     */
    @PostMapping("/logout-all")
    @Operation(summary = "Logout from all devices", description = "Logout user from all active sessions")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Logout from all devices successful"),
            @ApiResponse(responseCode = "401", description = "Unauthorized")
    })
    public ResponseEntity<Map<String, String>> logoutFromAllDevices(
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails,
            HttpServletRequest request,
            HttpServletResponse response) {
        log.info("Logout from all devices request received for user: {}", userDetails.getUsername());

        // Extract user ID from authenticated user
        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        authService.logoutFromAllDevices(userPrincipal.getId(), request, response);

        return ResponseEntity.ok(Map.of(
                "message", "Logout from all devices successful",
                "timestamp", java.time.LocalDateTime.now().toString()));
    }

    /**
     * Validate current session
     */
    @GetMapping("/validate")
    @Operation(summary = "Validate session", description = "Validate current user session")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Session valid"),
            @ApiResponse(responseCode = "401", description = "Session invalid")
    })
    public ResponseEntity<Map<String, Object>> validateSession(
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        if (userDetails != null) {
            com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

            return ResponseEntity.ok(Map.of(
                    "valid", true,
                    "user", Map.of(
                            "id", userPrincipal.getId(),
                            "email", userPrincipal.getUsername(),
                            "name", userPrincipal.getName(),
                            "status", userPrincipal.getStatus().getValue()),
                    "timestamp", java.time.LocalDateTime.now().toString()));
        }

        return ResponseEntity.ok(Map.of(
                "valid", false,
                "timestamp", java.time.LocalDateTime.now().toString()));
    }

    /**
     * Get current user info
     */
    @GetMapping("/me")
    @Operation(summary = "Get current user", description = "Get current authenticated user information")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "User information retrieved"),
            @ApiResponse(responseCode = "401", description = "Unauthorized")
    })
    public ResponseEntity<Map<String, Object>> getCurrentUser(
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        return ResponseEntity.ok(Map.of(
                "id", userPrincipal.getId(),
                "email", userPrincipal.getUsername(),
                "name", userPrincipal.getName(),
                "status", userPrincipal.getStatus().getValue(),
                "authorities", userPrincipal.getAuthorities().stream()
                        .map(auth -> auth.getAuthority())
                        .toList(),
                "timestamp", java.time.LocalDateTime.now().toString()));
    }

    /**
     * Health check endpoint
     */
    @GetMapping("/health")
    @Operation(summary = "Health check", description = "Check auth service health")
    public ResponseEntity<Map<String, String>> health() {
        return ResponseEntity.ok(Map.of(
                "status", "UP",
                "service", "auth-service",
                "timestamp", java.time.LocalDateTime.now().toString()));
    }
}