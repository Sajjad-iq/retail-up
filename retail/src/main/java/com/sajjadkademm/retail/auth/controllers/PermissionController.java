package com.sajjadkademm.retail.auth.controllers;

import com.sajjadkademm.retail.auth.dto.PermissionResponse;
import com.sajjadkademm.retail.auth.entities.Permission;
import com.sajjadkademm.retail.auth.services.PermissionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Permission management controller for handling permission operations.
 * Provides endpoints for permission administration.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/permissions")
@RequiredArgsConstructor
@Tag(name = "Permission Management", description = "Permission management endpoints")
@SecurityRequirement(name = "Bearer Authentication")
public class PermissionController {

    private final PermissionService permissionService;

    /**
     * Get all permissions
     */
    @GetMapping
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get all permissions", description = "Retrieve all system permissions")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Permissions retrieved successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<List<PermissionResponse>> getAllPermissions() {
        List<PermissionResponse> permissions = permissionService.getAllPermissions();
        return ResponseEntity.ok(permissions);
    }

    /**
     * Get permission by ID
     */
    @GetMapping("/{id}")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get permission by ID", description = "Retrieve permission by its unique identifier")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Permission found"),
            @ApiResponse(responseCode = "404", description = "Permission not found"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<PermissionResponse> getPermissionById(
            @Parameter(description = "Permission ID") @PathVariable String id) {
        PermissionResponse permission = permissionService.getPermissionById(id);
        return ResponseEntity.ok(permission);
    }

    /**
     * Get permissions by category
     */
    @GetMapping("/category/{category}")
    @PreAuthorize("hasAuthority('users.view') or hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get permissions by category", description = "Retrieve permissions filtered by category")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Permissions retrieved successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<List<PermissionResponse>> getPermissionsByCategory(
            @Parameter(description = "Permission category") @PathVariable Permission.PermissionCategory category) {
        List<PermissionResponse> permissions = permissionService.getPermissionsByCategory(category);
        return ResponseEntity.ok(permissions);
    }

    /**
     * Get system permissions
     */
    @GetMapping("/system")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get system permissions", description = "Retrieve all system permissions")
    public ResponseEntity<List<PermissionResponse>> getSystemPermissions() {
        List<PermissionResponse> permissions = permissionService.getSystemPermissions();
        return ResponseEntity.ok(permissions);
    }

    /**
     * Search permissions
     */
    @GetMapping("/search")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Search permissions", description = "Search permissions by name, label, or description")
    public ResponseEntity<List<PermissionResponse>> searchPermissions(
            @Parameter(description = "Search query") @RequestParam String query) {
        List<PermissionResponse> permissions = permissionService.searchPermissions(query);
        return ResponseEntity.ok(permissions);
    }

    /**
     * Get unassigned permissions
     */
    @GetMapping("/unassigned")
    @PreAuthorize("hasAuthority('users.manage') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get unassigned permissions", description = "Retrieve permissions not assigned to any user")
    public ResponseEntity<List<PermissionResponse>> getUnassignedPermissions() {
        List<PermissionResponse> permissions = permissionService.getUnassignedPermissions();
        return ResponseEntity.ok(permissions);
    }
}