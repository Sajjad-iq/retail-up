package com.sajjadkademm.retail.auth.services;

import com.sajjadkademm.retail.auth.dto.CreateUserRequest;
import com.sajjadkademm.retail.auth.dto.UserResponse;
import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.repositories.UserRepository;
import com.sajjadkademm.retail.exceptions.UserNotFoundException;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Service for user management operations.
 * Handles user CRUD, permissions, and business logic.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    /**
     * Get all users with pagination
     */
    @Transactional(readOnly = true)
    public Page<UserResponse> getAllUsers(int page, int size, String sortBy, String sortDirection) {
        Sort.Direction direction = Sort.Direction.fromString(sortDirection);
        Pageable pageable = PageRequest.of(page, size, Sort.by(direction, sortBy));

        Page<User> users = userRepository.findAll(pageable);
        return users.map(this::mapToUserResponse);
    }

    /**
     * Get user by ID
     */
    @Transactional(readOnly = true)
    public UserResponse getUserById(String id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new UserNotFoundException("User not found with id: " + id));
        return mapToUserResponse(user);
    }

    /**
     * Get user by email
     */
    @Transactional(readOnly = true)
    public UserResponse getUserByEmail(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new UserNotFoundException("User not found with email: " + email));
        return mapToUserResponse(user);
    }

    /**
     * Create new user
     */
    @Transactional
    public UserResponse createUser(CreateUserRequest request, String createdByUserId) {
        log.info("Creating user with email: {}", request.getEmail());

        // Check if email already exists
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new IllegalArgumentException("User with email " + request.getEmail() + " already exists");
        }

        // Check if employee ID already exists
        if (request.getEmployeeId() != null && userRepository.existsByEmployeeId(request.getEmployeeId())) {
            throw new IllegalArgumentException("User with employee ID " + request.getEmployeeId() + " already exists");
        }

        // Get permissions
        Set<Permission> permissions = getPermissionsByIds(request.getPermissionIds());

        // Create user entity
        User user = User.builder()
                .id(AuthUtils.generateUserId())
                .name(request.getName().trim())
                .email(request.getEmail().toLowerCase().trim())
                .password(passwordEncoder.encode(request.getPassword()))
                .phone(request.getPhone())
                .department(request.getDepartment())
                .employeeId(request.getEmployeeId())
                .status(request.getStatus())
                .mustChangePassword(request.getMustChangePassword())
                .permissions(permissions)
                .build();

        User savedUser = userRepository.save(user);

        // Log activity
        userActivityService.logActivity(
                createdByUserId,
                UserActivity.UserAction.CREATE_USER,
                "user:" + savedUser.getId(),
                "Created user: " + savedUser.getName(),
                null,
                null);

        log.info("User created successfully: {}", savedUser.getEmail());
        return mapToUserResponse(savedUser);
    }

    /**
     * Update user
     */
    @Transactional
    public UserResponse updateUser(String userId, CreateUserRequest request, String updatedByUserId) {
        log.info("Updating user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found with id: " + userId));

        // Check email uniqueness (if changed)
        if (!user.getEmail().equals(request.getEmail()) && userRepository.existsByEmail(request.getEmail())) {
            throw new IllegalArgumentException("User with email " + request.getEmail() + " already exists");
        }

        // Check employee ID uniqueness (if changed)
        if (request.getEmployeeId() != null &&
                !request.getEmployeeId().equals(user.getEmployeeId()) &&
                userRepository.existsByEmployeeId(request.getEmployeeId())) {
            throw new IllegalArgumentException("User with employee ID " + request.getEmployeeId() + " already exists");
        }

        // Update fields
        user.setName(request.getName().trim());
        user.setEmail(request.getEmail().toLowerCase().trim());
        user.setPhone(request.getPhone());
        user.setDepartment(request.getDepartment());
        user.setEmployeeId(request.getEmployeeId());
        user.setStatus(request.getStatus());
        user.setMustChangePassword(request.getMustChangePassword());

        // Update password if provided
        if (request.getPassword() != null && !request.getPassword().isEmpty()) {
            user.setPassword(passwordEncoder.encode(request.getPassword()));
        }

        // Update permissions
        Set<Permission> permissions = getPermissionsByIds(request.getPermissionIds());
        user.setPermissions(permissions);

        User savedUser = userRepository.save(user);

        // Log activity
        userActivityService.logActivity(
                updatedByUserId,
                UserActivity.UserAction.UPDATE_USER,
                "user:" + savedUser.getId(),
                "Updated user: " + savedUser.getName(),
                null,
                null);

        log.info("User updated successfully: {}", savedUser.getEmail());
        return mapToUserResponse(savedUser);
    }

    /**
     * Delete user
     */
    @Transactional
    public void deleteUser(String userId, String deletedByUserId) {
        log.info("Deleting user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found with id: " + userId));

        // Log activity before deletion
        userActivityService.logActivity(
                deletedByUserId,
                UserActivity.UserAction.DELETE_USER,
                "user:" + user.getId(),
                "Deleted user: " + user.getName(),
                null,
                null);

        userRepository.delete(user);
        log.info("User deleted successfully: {}", user.getEmail());
    }

    /**
     * Search users with filters
     */
    @Transactional(readOnly = true)
    public Page<UserResponse> searchUsers(String query, User.UserStatus status, String department,
            int page, int size, String sortBy, String sortDirection) {
        Sort.Direction direction = Sort.Direction.fromString(sortDirection);
        Pageable pageable = PageRequest.of(page, size, Sort.by(direction, sortBy));

        Page<User> users = userRepository.findUsersWithFilters(query, status, department, pageable);
        return users.map(this::mapToUserResponse);
    }

    /**
     * Get users by status
     */
    @Transactional(readOnly = true)
    public List<UserResponse> getUsersByStatus(User.UserStatus status) {
        List<User> users = userRepository.findByStatus(status);
        return users.stream()
                .map(this::mapToUserResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get users by department
     */
    @Transactional(readOnly = true)
    public List<UserResponse> getUsersByDepartment(String department) {
        List<User> users = userRepository.findByDepartment(department);
        return users.stream()
                .map(this::mapToUserResponse)
                .collect(Collectors.toList());
    }

    /**
     * Change user password
     */
    @Transactional
    public void changePassword(String userId, String newPassword, String changedByUserId) {
        log.info("Changing password for user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found with id: " + userId));

        user.setPassword(passwordEncoder.encode(newPassword));
        user.setMustChangePassword(false);
        userRepository.save(user);

        // Log activity
        userActivityService.logActivity(
                changedByUserId,
                UserActivity.UserAction.CHANGE_PASSWORD,
                "user:" + user.getId(),
                "Password changed for user: " + user.getName(),
                null,
                null);

        log.info("Password changed successfully for user: {}", user.getEmail());
    }

    /**
     * Get active users count
     */
    @Transactional(readOnly = true)
    public long getActiveUsersCount() {
        return userRepository.countByStatus(User.UserStatus.ACTIVE);
    }

    /**
     * Get users created this month
     */
    @Transactional(readOnly = true)
    public long getUsersCreatedThisMonth() {
        LocalDateTime startOfMonth = LocalDateTime.now().withDayOfMonth(1).withHour(0).withMinute(0).withSecond(0);
        return userRepository.countUsersCreatedAfter(startOfMonth);
    }

    /**
     * Get users logged in today
     */
    @Transactional(readOnly = true)
    public long getUsersLoggedInToday() {
        LocalDateTime startOfDay = LocalDateTime.now().withHour(0).withMinute(0).withSecond(0);
        return userRepository.countUsersLoggedInAfter(startOfDay);
    }

    /**
     * Get most active users
     */
    @Transactional(readOnly = true)
    public List<UserResponse> getMostActiveUsers(int limit) {
        Pageable pageable = PageRequest.of(0, limit);
        List<User> users = userRepository.findMostActiveUsers(pageable);
        return users.stream()
                .map(this::mapToUserResponse)
                .collect(Collectors.toList());
    }

    /**
     * Map User entity to UserResponse DTO
     */
    public UserResponse mapToUserResponse(User user) {
        List<PermissionResponse> permissions = user.getPermissions().stream()
                .map(this::mapToPermissionResponse)
                .collect(Collectors.toList());

        return UserResponse.builder()
                .id(user.getId())
                .name(user.getName())
                .email(user.getEmail())
                .phone(user.getPhone())
                .avatar(user.getAvatar())
                .status(user.getStatus())
                .department(user.getDepartment())
                .employeeId(user.getEmployeeId())
                .permissions(permissions)
                .mustChangePassword(user.getMustChangePassword())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .lastLoginAt(user.getLastLoginAt())
                .permissionCount(user.getPermissions().size())
                .build();
    }

    /**
     * Map Permission entity to PermissionResponse DTO
     */
    private PermissionResponse mapToPermissionResponse(Permission permission) {
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

    /**
     * Get permissions by IDs
     */
    private Set<Permission> getPermissionsByIds(List<String> permissionIds) {
        List<Permission> permissions = permissionRepository.findByIdIn(permissionIds);

        if (permissions.size() != permissionIds.size()) {
            throw new IllegalArgumentException("One or more permission IDs are invalid");
        }

        return permissions.stream().collect(Collectors.toSet());
    }
}