package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.service.RoleService;
import com.retails.retail.common.dto.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Role Management Controller
 * Handles role CRUD operations, permission management, and role analytics
 */
@RestController
@RequestMapping("/api/auth/roles")
@RequiredArgsConstructor
@Slf4j
public class RoleController {

    private final RoleService roleService;

    /**
     * Get all roles with pagination and filtering
     */
    @GetMapping
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<Page<RoleDto>>> getRoles(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "ASC") String sortDirection,
            @RequestParam(required = false) String search) {

        log.info("Get roles request - page: {}, size: {}, search: {}", page, size, search);

        try {
            Page<RoleDto> roles = roleService.getRoles(page, size, sortBy, sortDirection, search);
            return ResponseEntity.ok(ApiResponse.success(roles));
        } catch (Exception e) {
            log.error("Error fetching roles", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch roles", "ROLE_001"));
        }
    }

    /**
     * Get role by ID
     */
    @GetMapping("/{roleId}")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<RoleDto>> getRoleById(@PathVariable UUID roleId) {
        log.info("Get role request for ID: {}", roleId);

        try {
            RoleDto role = roleService.getRoleById(roleId);
            return ResponseEntity.ok(ApiResponse.success(role));
        } catch (IllegalArgumentException e) {
            log.error("Role not found: {}", roleId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Role not found", "ROLE_002"));
        } catch (Exception e) {
            log.error("Error fetching role: {}", roleId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch role", "ROLE_003"));
        }
    }

    /**
     * Create new role
     */
    @PostMapping
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<RoleDto>> createRole(
            @Valid @RequestBody CreateRoleRequest request,
            HttpServletRequest httpRequest) {

        log.info("Create role request for name: {}", request.getName());

        try {
            RoleDto role = roleService.createRole(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(role, "Role created successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error creating role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_004"));
        } catch (Exception e) {
            log.error("Error creating role", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to create role", "ROLE_005"));
        }
    }

    /**
     * Update role
     */
    @PutMapping("/{roleId}")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<RoleDto>> updateRole(
            @PathVariable UUID roleId,
            @Valid @RequestBody CreateRoleRequest request,
            HttpServletRequest httpRequest) {

        log.info("Update role request for ID: {}", roleId);

        try {
            RoleDto role = roleService.updateRole(roleId, request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(role, "Role updated successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error updating role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_006"));
        } catch (Exception e) {
            log.error("Error updating role: {}", roleId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to update role", "ROLE_007"));
        }
    }

    /**
     * Delete role
     */
    @DeleteMapping("/{roleId}")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<String>> deleteRole(
            @PathVariable UUID roleId,
            HttpServletRequest httpRequest) {

        log.info("Delete role request for ID: {}", roleId);

        try {
            roleService.deleteRole(roleId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Role deleted successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error deleting role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_008"));
        } catch (Exception e) {
            log.error("Error deleting role: {}", roleId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to delete role", "ROLE_009"));
        }
    }

    /**
     * Get all permissions
     */
    @GetMapping("/permissions")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<List<PermissionDto>>> getAllPermissions() {
        log.info("Get all permissions request");

        try {
            List<PermissionDto> permissions = roleService.getAllPermissions();
            return ResponseEntity.ok(ApiResponse.success(permissions));
        } catch (Exception e) {
            log.error("Error fetching permissions", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch permissions", "ROLE_010"));
        }
    }

    /**
     * Assign role to user
     */
    @PostMapping("/{roleId}/assign/{userId}")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<String>> assignRoleToUser(
            @PathVariable UUID roleId,
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Assign role {} to user {} request", roleId, userId);

        try {
            roleService.assignRoleToUser(userId, roleId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Role assigned successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error assigning role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_011"));
        } catch (Exception e) {
            log.error("Error assigning role {} to user {}", roleId, userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to assign role", "ROLE_012"));
        }
    }

    /**
     * Remove role from user
     */
    @DeleteMapping("/{roleId}/remove/{userId}")
    @PreAuthorize("hasPermission('users', 'roles')")
    public ResponseEntity<ApiResponse<String>> removeRoleFromUser(
            @PathVariable UUID roleId,
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Remove role {} from user {} request", roleId, userId);

        try {
            roleService.removeRoleFromUser(userId, roleId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Role removed successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Error removing role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_013"));
        } catch (Exception e) {
            log.error("Error removing role {} from user {}", roleId, userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to remove role", "ROLE_014"));
        }
    }

    /**
     * Get users with specific role
     */
    @GetMapping("/{roleId}/users")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Page<UserDto>>> getUsersByRole(
            @PathVariable UUID roleId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        log.info("Get users by role {} request", roleId);

        try {
            Page<UserDto> users = roleService.getUsersByRole(roleId, page, size);
            return ResponseEntity.ok(ApiResponse.success(users));
        } catch (IllegalArgumentException e) {
            log.error("Error fetching users by role: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "ROLE_015"));
        } catch (Exception e) {
            log.error("Error fetching users by role: {}", roleId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch users by role", "ROLE_016"));
        }
    }

    /**
     * Get role statistics
     */
    @GetMapping("/statistics")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getRoleStatistics() {
        log.info("Role statistics request");

        try {
            Map<String, Object> stats = roleService.getRoleStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching role statistics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch role statistics", "ROLE_017"));
        }
    }
}