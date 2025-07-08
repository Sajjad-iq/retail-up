package com.sajjadkademm.retail.auth.controllers;

import com.sajjadkademm.retail.auth.dto.CreateUserRequest;
import com.sajjadkademm.retail.auth.dto.UserResponse;
import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.services.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * User management controller for handling user CRUD operations.
 * Provides endpoints for user administration.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
@Tag(name = "User Management", description = "User management endpoints")
@SecurityRequirement(name = "Bearer Authentication")
public class UserController {

    private final UserService userService;

    /**
     * Get all users with pagination
     */
    @GetMapping
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get all users", description = "Retrieve all users with pagination and sorting")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Users retrieved successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<Page<UserResponse>> getAllUsers(
            @Parameter(description = "Page number (0-based)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "10") int size,
            @Parameter(description = "Sort field") @RequestParam(defaultValue = "name") String sortBy,
            @Parameter(description = "Sort direction") @RequestParam(defaultValue = "asc") String sortDirection) {

        Page<UserResponse> users = userService.getAllUsers(page, size, sortBy, sortDirection);
        return ResponseEntity.ok(users);
    }

    /**
     * Get user by ID
     */
    @GetMapping("/{id}")
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get user by ID", description = "Retrieve user by their unique identifier")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "User found"),
            @ApiResponse(responseCode = "404", description = "User not found"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<UserResponse> getUserById(
            @Parameter(description = "User ID") @PathVariable String id) {

        UserResponse user = userService.getUserById(id);
        return ResponseEntity.ok(user);
    }

    /**
     * Create new user
     */
    @PostMapping
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Create user", description = "Create a new user account")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "User created successfully"),
            @ApiResponse(responseCode = "400", description = "Invalid request data"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden"),
            @ApiResponse(responseCode = "409", description = "User already exists")
    })
    public ResponseEntity<UserResponse> createUser(
            @Valid @RequestBody CreateUserRequest request,
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        UserResponse createdUser = userService.createUser(request, userPrincipal.getId());
        return ResponseEntity.status(HttpStatus.CREATED).body(createdUser);
    }

    /**
     * Update user
     */
    @PutMapping("/{id}")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Update user", description = "Update existing user information")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "User updated successfully"),
            @ApiResponse(responseCode = "400", description = "Invalid request data"),
            @ApiResponse(responseCode = "404", description = "User not found"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<UserResponse> updateUser(
            @Parameter(description = "User ID") @PathVariable String id,
            @Valid @RequestBody CreateUserRequest request,
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        UserResponse updatedUser = userService.updateUser(id, request, userPrincipal.getId());
        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Delete user
     */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Delete user", description = "Delete user account")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "User deleted successfully"),
            @ApiResponse(responseCode = "404", description = "User not found"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<Map<String, String>> deleteUser(
            @Parameter(description = "User ID") @PathVariable String id,
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        userService.deleteUser(id, userPrincipal.getId());

        return ResponseEntity.ok(Map.of(
                "message", "User deleted successfully",
                "userId", id,
                "timestamp", java.time.LocalDateTime.now().toString()));
    }

    /**
     * Search users
     */
    @GetMapping("/search")
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Search users", description = "Search users with filters")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Search completed successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<Page<UserResponse>> searchUsers(
            @Parameter(description = "Search query") @RequestParam(required = false) String query,
            @Parameter(description = "User status filter") @RequestParam(required = false) User.UserStatus status,
            @Parameter(description = "Department filter") @RequestParam(required = false) String department,
            @Parameter(description = "Page number") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "10") int size,
            @Parameter(description = "Sort field") @RequestParam(defaultValue = "name") String sortBy,
            @Parameter(description = "Sort direction") @RequestParam(defaultValue = "asc") String sortDirection) {

        Page<UserResponse> users = userService.searchUsers(query, status, department, page, size, sortBy,
                sortDirection);
        return ResponseEntity.ok(users);
    }

    /**
     * Get users by status
     */
    @GetMapping("/status/{status}")
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get users by status", description = "Retrieve users filtered by status")
    public ResponseEntity<List<UserResponse>> getUsersByStatus(
            @Parameter(description = "User status") @PathVariable User.UserStatus status) {

        List<UserResponse> users = userService.getUsersByStatus(status);
        return ResponseEntity.ok(users);
    }

    /**
     * Get users by department
     */
    @GetMapping("/department/{department}")
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get users by department", description = "Retrieve users filtered by department")
    public ResponseEntity<List<UserResponse>> getUsersByDepartment(
            @Parameter(description = "Department name") @PathVariable String department) {

        List<UserResponse> users = userService.getUsersByDepartment(department);
        return ResponseEntity.ok(users);
    }

    /**
     * Change user password
     */
    @PostMapping("/{id}/change-password")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Change user password", description = "Change password for specific user")
    public ResponseEntity<Map<String, String>> changePassword(
            @Parameter(description = "User ID") @PathVariable String id,
            @RequestBody Map<String, String> request,
            @AuthenticationPrincipal org.springframework.security.core.userdetails.UserDetails userDetails) {

        com.sajjadkademm.retail.auth.security.UserPrincipal userPrincipal = (com.sajjadkademm.retail.auth.security.UserPrincipal) userDetails;

        String newPassword = request.get("newPassword");
        if (newPassword == null || newPassword.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("error", "New password is required"));
        }

        userService.changePassword(id, newPassword, userPrincipal.getId());

        return ResponseEntity.ok(Map.of(
                "message", "Password changed successfully",
                "userId", id,
                "timestamp", java.time.LocalDateTime.now().toString()));
    }
}