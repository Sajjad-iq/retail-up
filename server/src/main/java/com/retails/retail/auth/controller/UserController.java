package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.service.UserService;
import com.retails.retail.common.dto.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
import java.util.UUID;

/**
 * User Management Controller
 * Handles user CRUD operations, search, filtering, and bulk operations
 */
@RestController
@RequestMapping("/api/auth/users")
@RequiredArgsConstructor
@Slf4j
public class UserController {

    private final UserService userService;

    /**
     * Get all users with pagination and filtering
     */
    @GetMapping
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Page<UserDto>>> getUsers(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "DESC") String sortDirection,
            @RequestParam(required = false) String query) {

        log.info("Get users request - page: {}, size: {}, query: {}", page, size, query);

        try {
            UserSearchRequest searchRequest = new UserSearchRequest();
            searchRequest.setPage(page);
            searchRequest.setSize(size);
            searchRequest.setSortBy(sortBy);
            searchRequest.setSortDirection(sortDirection);
            searchRequest.setQuery(query);

            Page<UserDto> users = userService.getUsers(searchRequest);
            return ResponseEntity.ok(ApiResponse.success(users));
        } catch (Exception e) {
            log.error("Error fetching users", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch users", "USER_001"));
        }
    }

    /**
     * Get user by ID
     */
    @GetMapping("/{userId}")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<UserDto>> getUserById(@PathVariable UUID userId) {
        log.info("Get user request for ID: {}", userId);

        try {
            UserDto user = userService.getUserById(userId);
            return ResponseEntity.ok(ApiResponse.success(user));
        } catch (IllegalArgumentException e) {
            log.error("User not found: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("User not found", "USER_002"));
        } catch (Exception e) {
            log.error("Error fetching user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch user", "USER_003"));
        }
    }

    /**
     * Create new user
     */
    @PostMapping
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<UserDto>> createUser(
            @Valid @RequestBody CreateUserRequest request,
            HttpServletRequest httpRequest) {

        log.info("Create user request for email: {}", request.getEmail());

        try {
            UserDto user = userService.createUser(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(user, "User created successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error creating user: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_004"));
        } catch (Exception e) {
            log.error("Error creating user", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to create user", "USER_005"));
        }
    }

    /**
     * Update user
     */
    @PutMapping("/{userId}")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<UserDto>> updateUser(
            @PathVariable UUID userId,
            @Valid @RequestBody UpdateUserRequest request,
            HttpServletRequest httpRequest) {

        log.info("Update user request for ID: {}", userId);

        try {
            UserDto user = userService.updateUser(userId, request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(user, "User updated successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error updating user: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_006"));
        } catch (Exception e) {
            log.error("Error updating user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to update user", "USER_007"));
        }
    }

    /**
     * Delete user
     */
    @DeleteMapping("/{userId}")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> deleteUser(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Delete user request for ID: {}", userId);

        try {
            userService.deleteUser(userId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("User deleted successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error deleting user: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_008"));
        } catch (Exception e) {
            log.error("Error deleting user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to delete user", "USER_009"));
        }
    }

    /**
     * Activate user
     */
    @PostMapping("/{userId}/activate")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> activateUser(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Activate user request for ID: {}", userId);

        try {
            userService.activateUser(userId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("User activated successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error activating user: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_010"));
        } catch (Exception e) {
            log.error("Error activating user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to activate user", "USER_011"));
        }
    }

    /**
     * Deactivate user
     */
    @PostMapping("/{userId}/deactivate")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> deactivateUser(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Deactivate user request for ID: {}", userId);

        try {
            userService.deactivateUser(userId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("User deactivated successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error deactivating user: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_012"));
        } catch (Exception e) {
            log.error("Error deactivating user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to deactivate user", "USER_013"));
        }
    }

    /**
     * Perform bulk operations on users
     */
    @PostMapping("/bulk")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> performBulkOperation(
            @Valid @RequestBody BulkUserOperationDto request,
            HttpServletRequest httpRequest) {

        log.info("Bulk operation request: {} for {} users", request.getOperation(), request.getUserIds().size());

        try {
            Map<String, Object> result = userService.performBulkOperation(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(result, "Bulk operation completed"));
        } catch (IllegalArgumentException e) {
            log.error("Error in bulk operation: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "USER_014"));
        } catch (Exception e) {
            log.error("Error in bulk operation", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to perform bulk operation", "USER_015"));
        }
    }

    /**
     * Get user statistics
     */
    @GetMapping("/statistics")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getUserStatistics() {
        log.info("User statistics request");

        try {
            Map<String, Object> stats = userService.getUserStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching user statistics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch user statistics", "USER_016"));
        }
    }
}