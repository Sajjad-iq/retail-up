package com.retails.retail.auth.service;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.entity.Permission;
import com.retails.retail.auth.entity.Role;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.repository.PermissionRepository;
import com.retails.retail.auth.repository.RoleRepository;
import com.retails.retail.auth.repository.UserRepository;
import com.retails.retail.auth.repository.UserActivityRepository;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.Predicate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Service for role and permission management
 * Handles role CRUD operations, permission assignment, and role analytics
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RoleService {

    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;
    private final UserRepository userRepository;
    private final UserActivityRepository userActivityRepository;

    /**
     * Get all roles with pagination and filtering
     */
    public Page<RoleDto> getRoles(int page, int size, String sortBy, String sortDirection, String search) {
        log.info("Fetching roles - page: {}, size: {}, search: {}", page, size, search);

        Sort sort = Sort.by(Sort.Direction.fromString(sortDirection), sortBy);
        Pageable pageable = PageRequest.of(page, size, sort);

        Specification<Role> spec = (root, query, criteriaBuilder) -> {
            List<Predicate> predicates = new ArrayList<>();
            predicates.add(criteriaBuilder.isFalse(root.get("isDeleted")));

            if (search != null && !search.trim().isEmpty()) {
                String searchTerm = "%" + search.toLowerCase() + "%";
                Predicate namePredicate = criteriaBuilder.like(criteriaBuilder.lower(root.get("name")), searchTerm);
                Predicate descPredicate = criteriaBuilder.like(criteriaBuilder.lower(root.get("description")),
                        searchTerm);
                predicates.add(criteriaBuilder.or(namePredicate, descPredicate));
            }

            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        Page<Role> roles = roleRepository.findAll(spec, pageable);
        return roles.map(RoleDto::fromEntity);
    }

    /**
     * Get role by ID
     */
    public RoleDto getRoleById(UUID roleId) {
        log.info("Fetching role by ID: {}", roleId);

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found with ID: " + roleId));

        return RoleDto.fromEntity(role);
    }

    /**
     * Create new role
     */
    public RoleDto createRole(CreateRoleRequest request, HttpServletRequest httpRequest) {
        log.info("Creating new role: {}", request.getName());

        // Check if role name already exists
        if (roleRepository.existsByNameIgnoreCase(request.getName())) {
            throw new IllegalArgumentException("Role with this name already exists");
        }

        // Get permissions
        Set<Permission> permissions = new HashSet<>();
        if (request.getPermissionIds() != null && !request.getPermissionIds().isEmpty()) {
            permissions = permissionRepository.findAllById(request.getPermissionIds())
                    .stream().collect(Collectors.toSet());
        }

        // Create role
        Role role = Role.builder()
                .name(request.getName())
                .description(request.getDescription())
                .isSystem(false) // User-created roles are never system roles
                .permissions(permissions)
                .createdAt(LocalDateTime.now())
                .build();

        role = roleRepository.save(role);

        // Log activity
        logActivity(UserActivity.UserAction.CREATE_ROLE, "Role",
                "Role created: " + role.getName(), httpRequest);

        log.info("Role created successfully: {}", role.getName());
        return RoleDto.fromEntity(role);
    }

    /**
     * Update role
     */
    public RoleDto updateRole(UUID roleId, CreateRoleRequest request, HttpServletRequest httpRequest) {
        log.info("Updating role: {}", roleId);

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        // Prevent modification of system roles
        if (role.getIsSystem()) {
            throw new IllegalArgumentException("Cannot modify system roles");
        }

        // Check name uniqueness if changed
        if (!role.getName().equalsIgnoreCase(request.getName()) &&
                roleRepository.existsByNameIgnoreCase(request.getName())) {
            throw new IllegalArgumentException("Role with this name already exists");
        }

        // Update role
        role.setName(request.getName());
        role.setDescription(request.getDescription());

        // Update permissions
        Set<Permission> permissions = new HashSet<>();
        if (request.getPermissionIds() != null && !request.getPermissionIds().isEmpty()) {
            permissions = permissionRepository.findAllById(request.getPermissionIds())
                    .stream().collect(Collectors.toSet());
        }
        role.setPermissions(permissions);

        role = roleRepository.save(role);

        // Log activity
        logActivity(UserActivity.UserAction.UPDATE_ROLE, "Role",
                "Role updated: " + role.getName(), httpRequest);

        log.info("Role updated successfully: {}", role.getName());
        return RoleDto.fromEntity(role);
    }

    // Private helper methods
    private void logActivity(UserActivity.UserAction action, String resource, String details,
            HttpServletRequest request) {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth != null && auth.isAuthenticated()) {
                String email = auth.getName();
                User user = userRepository.findByEmailIgnoreCase(email).orElse(null);
                if (user != null) {
                    UserActivity activity = UserActivity.builder()
                            .userId(user.getId())
                            .action(action)
                            .resource(resource)
                            .details(details)
                            .ipAddress(getClientIpAddress(request))
                            .userAgent(getUserAgent(request))
                            .timestamp(LocalDateTime.now())
                            .build();
                    userActivityRepository.save(activity);
                }
            }
        } catch (Exception e) {
            log.error("Failed to log activity", e);
        }
    }

    private String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
            return xForwardedFor.split(",")[0].trim();
        }
        return request.getRemoteAddr();
    }

    private String getUserAgent(HttpServletRequest request) {
        return request.getHeader("User-Agent");
    }

    /**
     * Get role statistics
     */
    public Map<String, Object> getRoleStatistics() {
        log.info("Generating role statistics");

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalRoles", roleRepository.countByIsDeletedFalse());
        stats.put("systemRoles", roleRepository.countByIsSystemTrueAndIsDeletedFalse());
        stats.put("customRoles", roleRepository.countByIsSystemFalseAndIsDeletedFalse());

        return stats;
    }

    /**
     * Delete role (soft delete)
     */
    public void deleteRole(UUID roleId, HttpServletRequest httpRequest) {
        log.info("Deleting role: {}", roleId);

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        // Prevent deletion of system roles
        if (role.getIsSystem()) {
            throw new IllegalArgumentException("Cannot delete system roles");
        }

        // Check if role is assigned to users
        long userCount = userRepository.countByRolesContainingAndIsDeletedFalse(role);
        if (userCount > 0) {
            throw new IllegalArgumentException("Cannot delete role that is assigned to " + userCount + " user(s)");
        }

        role.setIsDeleted(true);
        role.setDeletedAt(LocalDateTime.now());
        roleRepository.save(role);

        // Log activity
        logActivity(UserActivity.UserAction.DELETE_ROLE, "Role",
                "Role deleted: " + role.getName(), httpRequest);

        log.info("Role deleted successfully: {}", role.getName());
    }

    /**
     * Get all permissions
     */
    public List<PermissionDto> getAllPermissions() {
        log.info("Fetching all permissions");

        List<Permission> permissions = permissionRepository.findAllOrderedByCategoryAndLabel();
        return permissions.stream()
                .map(PermissionDto::fromEntity)
                .collect(Collectors.toList());
    }

    /**
     * Assign role to user
     */
    public void assignRoleToUser(UUID userId, UUID roleId, HttpServletRequest httpRequest) {
        log.info("Assigning role {} to user {}", roleId, userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        if (user.getRoles().contains(role)) {
            throw new IllegalArgumentException("User already has this role");
        }

        user.getRoles().add(role);
        userRepository.save(user);

        // Log activity
        logActivity(UserActivity.UserAction.ASSIGN_ROLE, "User",
                "Role '" + role.getName() + "' assigned to user: " + user.getEmail(), httpRequest);

        log.info("Role '{}' assigned to user '{}' successfully", role.getName(), user.getEmail());
    }

    /**
     * Remove role from user
     */
    public void removeRoleFromUser(UUID userId, UUID roleId, HttpServletRequest httpRequest) {
        log.info("Removing role {} from user {}", roleId, userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        if (!user.getRoles().contains(role)) {
            throw new IllegalArgumentException("User does not have this role");
        }

        // Ensure user has at least one role
        if (user.getRoles().size() <= 1) {
            throw new IllegalArgumentException("User must have at least one role");
        }

        user.getRoles().remove(role);
        userRepository.save(user);

        // Log activity
        logActivity(UserActivity.UserAction.REMOVE_ROLE, "User",
                "Role '" + role.getName() + "' removed from user: " + user.getEmail(), httpRequest);

        log.info("Role '{}' removed from user '{}' successfully", role.getName(), user.getEmail());
    }

    /**
     * Get users with specific role
     */
    public Page<UserDto> getUsersByRole(UUID roleId, int page, int size) {
        log.info("Fetching users with role: {}", roleId);

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        Pageable pageable = PageRequest.of(page, size, Sort.by("name"));
        Page<User> users = userRepository.findByRolesContainingAndIsDeletedFalse(role, pageable);

        return users.map(UserDto::fromEntity);
    }
}