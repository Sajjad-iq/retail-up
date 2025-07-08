package com.sajjadkademm.retail.auth.services;

import com.sajjadkademm.retail.auth.dto.PermissionResponse;
import com.sajjadkademm.retail.auth.entities.Permission;
import com.sajjadkademm.retail.auth.exceptions.PermissionNotFoundException;
import com.sajjadkademm.retail.auth.repositories.PermissionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Service for permission management operations.
 * Handles permission retrieval and business logic.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class PermissionService {

    private final PermissionRepository permissionRepository;

    /**
     * Get all permissions
     */
    @Transactional(readOnly = true)
    public List<PermissionResponse> getAllPermissions() {
        List<Permission> permissions = permissionRepository.findAll();
        return permissions.stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get permission by ID
     */
    @Transactional(readOnly = true)
    public PermissionResponse getPermissionById(String id) {
        Permission permission = permissionRepository.findById(id)
                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with id: " + id));
        return mapToPermissionResponse(permission);
    }

    /**
     * Get permission by name
     */
    @Transactional(readOnly = true)
    public PermissionResponse getPermissionByName(String name) {
        Permission permission = permissionRepository.findByName(name)
                .orElseThrow(() -> new PermissionNotFoundException("Permission not found with name: " + name));
        return mapToPermissionResponse(permission);
    }

    /**
     * Get permissions by category
     */
    @Transactional(readOnly = true)
    public List<PermissionResponse> getPermissionsByCategory(Permission.PermissionCategory category) {
        List<Permission> permissions = permissionRepository.findByCategory(category);
        return permissions.stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get system permissions
     */
    @Transactional(readOnly = true)
    public List<PermissionResponse> getSystemPermissions() {
        List<Permission> permissions = permissionRepository.findByIsSystemTrue();
        return permissions.stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());
    }

    /**
     * Search permissions
     */
    @Transactional(readOnly = true)
    public List<PermissionResponse> searchPermissions(String query) {
        List<Permission> permissions = permissionRepository.searchPermissions(query);
        return permissions.stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get permission distribution
     */
    @Transactional(readOnly = true)
    public List<Object[]> getPermissionDistribution() {
        return permissionRepository.findMostAssignedPermissions();
    }

    /**
     * Get unassigned permissions
     */
    @Transactional(readOnly = true)
    public List<PermissionResponse> getUnassignedPermissions() {
        List<Permission> permissions = permissionRepository.findUnassignedPermissions();
        return permissions.stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());
    }

    /**
     * Map Permission entity to PermissionResponse DTO
     */
    public PermissionResponse mapToPermissionResponse(Permission permission) {
        return PermissionResponse.builder()
                .id(permission.getId())
                .name(permission.getName())
                .label(permission.getLabel())
                .description(permission.getDescription())
                .category(permission.getCategory())
                .isSystem(permission.getIsSystem())
                .createdAt(permission.getCreatedAt())
                .updatedAt(permission.getUpdatedAt())
                .build();
    }
}