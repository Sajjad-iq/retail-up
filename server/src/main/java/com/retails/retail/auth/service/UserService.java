package com.retails.retail.auth.service;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.entity.Role;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
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
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.Predicate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Service for user management operations
 * Handles CRUD operations, search, filtering, and bulk operations
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class UserService extends BaseAuthService {

    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final UserActivityRepository userActivityRepository;
    private final PasswordEncoder passwordEncoder;

    /**
     * Get all users with pagination and filtering
     */
    public Page<UserDto> getUsers(UserSearchRequest request) {
        log.info("Fetching users with filters: {}", request);

        Specification<User> spec = buildUserSpecification(request);
        Sort sort = Sort.by(Sort.Direction.fromString(request.getSortDirection()), request.getSortBy());
        Pageable pageable = PageRequest.of(request.getPage(), request.getSize(), sort);

        Page<User> users = userRepository.findAll(spec, pageable);
        return users.map(UserDto::fromEntity);
    }

    /**
     * Get user by ID
     */
    public UserDto getUserById(UUID userId) {
        log.info("Fetching user by ID: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found with ID: " + userId));

        return UserDto.fromEntity(user);
    }

    /**
     * Create new user
     */
    public UserDto createUser(CreateUserRequest request, HttpServletRequest httpRequest) {
        log.info("Creating new user: {}", request.getEmail());

        // Check if email already exists
        if (userRepository.existsByEmailIgnoreCase(request.getEmail())) {
            throw new IllegalArgumentException("User with this email already exists");
        }

        // Get role
        Role role = roleRepository.findById(request.getRoleId())
                .orElseThrow(() -> new IllegalArgumentException("Role not found"));

        // Convert status string to enum
        User.UserStatus userStatus = request.getStatus() != null ? User.UserStatus.fromString(request.getStatus())
                : User.UserStatus.ACTIVE;

        // Create user
        User user = User.builder()
                .name(request.getName())
                .email(request.getEmail().toLowerCase())
                .phone(request.getPhone())
                .department(request.getDepartment())
                .employeeId(request.getEmployeeId())
                .passwordHash(passwordEncoder.encode(request.getPassword()))
                .mustChangePassword(request.getMustChangePassword() != null ? request.getMustChangePassword() : true)
                .status(userStatus)
                .roles(new HashSet<>(Collections.singletonList(role)))
                .createdAt(LocalDateTime.now())
                .build();

        user = userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.CREATE_USER, "User",
                "User created: " + user.getEmail(), httpRequest);

        log.info("User created successfully: {} ({})", user.getName(), user.getEmail());
        return UserDto.fromEntity(user);
    }

    /**
     * Update user
     */
    public UserDto updateUser(UUID userId, UpdateUserRequest request, HttpServletRequest httpRequest) {
        log.info("Updating user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Check email uniqueness if changed
        if (!user.getEmail().equalsIgnoreCase(request.getEmail()) &&
                userRepository.existsByEmailIgnoreCase(request.getEmail())) {
            throw new IllegalArgumentException("User with this email already exists");
        }

        // Update user fields
        user.setName(request.getName());
        user.setEmail(request.getEmail().toLowerCase());
        user.setPhone(request.getPhone());
        user.setDepartment(request.getDepartment());
        user.setEmployeeId(request.getEmployeeId());

        // Convert status string to enum if provided
        if (request.getStatus() != null) {
            user.setStatus(User.UserStatus.fromString(request.getStatus()));
        }

        // Update role if provided
        if (request.getRoleId() != null) {
            Role role = roleRepository.findById(request.getRoleId())
                    .orElseThrow(() -> new IllegalArgumentException("Role not found"));
            user.setRoles(new HashSet<>(Collections.singletonList(role)));
        }

        // Update password if provided
        if (request.getPassword() != null && !request.getPassword().trim().isEmpty()) {
            user.setPasswordHash(passwordEncoder.encode(request.getPassword()));
        }

        // Update mustChangePassword if provided
        if (request.getMustChangePassword() != null) {
            user.setMustChangePassword(request.getMustChangePassword());
        }

        user = userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.UPDATE_USER, "User",
                "User updated: " + user.getEmail(), httpRequest);

        log.info("User updated successfully: {} ({})", user.getName(), user.getEmail());
        return UserDto.fromEntity(user);
    }

    /**
     * Delete user (soft delete)
     */
    public void deleteUser(UUID userId, HttpServletRequest httpRequest) {
        log.info("Deleting user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Prevent self-deletion
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null && auth.getName().equalsIgnoreCase(user.getEmail())) {
            throw new IllegalArgumentException("Cannot delete your own account");
        }

        user.setIsDeleted(true);
        user.setDeletedAt(LocalDateTime.now());
        userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.DELETE_USER, "User",
                "User deleted: " + user.getEmail(), httpRequest);

        log.info("User deleted successfully: {} ({})", user.getName(), user.getEmail());
    }

    /**
     * Activate user
     */
    public void activateUser(UUID userId, HttpServletRequest httpRequest) {
        log.info("Activating user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        user.setStatus(User.UserStatus.ACTIVE);
        userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.ACTIVATE_USER, "User",
                "User activated: " + user.getEmail(), httpRequest);

        log.info("User activated successfully: {} ({})", user.getName(), user.getEmail());
    }

    /**
     * Deactivate user
     */
    public void deactivateUser(UUID userId, HttpServletRequest httpRequest) {
        log.info("Deactivating user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Prevent self-deactivation
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null && auth.getName().equalsIgnoreCase(user.getEmail())) {
            throw new IllegalArgumentException("Cannot deactivate your own account");
        }

        user.setStatus(User.UserStatus.INACTIVE);
        userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.DEACTIVATE_USER, "User",
                "User deactivated: " + user.getEmail(), httpRequest);

        log.info("User deactivated successfully: {} ({})", user.getName(), user.getEmail());
    }

    /**
     * Perform bulk operations on users
     */
    public Map<String, Object> performBulkOperation(BulkUserOperationDto request, HttpServletRequest httpRequest) {
        log.info("Performing bulk operation: {} on {} users", request.getOperation(), request.getUserIds().size());

        List<User> users = userRepository.findAllById(request.getUserIds());
        Map<String, Object> result = new HashMap<>();
        List<String> errors = new ArrayList<>();
        int successCount = 0;

        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        String currentUserEmail = auth != null ? auth.getName() : null;

        for (User user : users) {
            try {
                switch (request.getOperation()) {
                    case ACTIVATE:
                        user.setStatus(User.UserStatus.ACTIVE);
                        logActivity(user.getId(), UserActivity.UserAction.ACTIVATE_USER, "User",
                                "Bulk activation", httpRequest);
                        break;

                    case DEACTIVATE:
                        if (currentUserEmail != null && currentUserEmail.equalsIgnoreCase(user.getEmail())) {
                            errors.add("Cannot deactivate your own account: " + user.getEmail());
                            continue;
                        }
                        user.setStatus(User.UserStatus.INACTIVE);
                        logActivity(user.getId(), UserActivity.UserAction.DEACTIVATE_USER, "User",
                                "Bulk deactivation", httpRequest);
                        break;

                    case DELETE:
                        if (currentUserEmail != null && currentUserEmail.equalsIgnoreCase(user.getEmail())) {
                            errors.add("Cannot delete your own account: " + user.getEmail());
                            continue;
                        }
                        user.setIsDeleted(true);
                        user.setDeletedAt(LocalDateTime.now());
                        logActivity(user.getId(), UserActivity.UserAction.DELETE_USER, "User",
                                "Bulk deletion", httpRequest);
                        break;

                    case LOCK:
                        user.setStatus(User.UserStatus.LOCKED);
                        logActivity(user.getId(), UserActivity.UserAction.LOCK_USER, "User",
                                "Bulk lock", httpRequest);
                        break;

                    case UNLOCK:
                        user.setStatus(User.UserStatus.ACTIVE);
                        logActivity(user.getId(), UserActivity.UserAction.UNLOCK_USER, "User",
                                "Bulk unlock", httpRequest);
                        break;

                    case FORCE_PASSWORD_CHANGE:
                        user.setMustChangePassword(true);
                        logActivity(user.getId(), UserActivity.UserAction.FORCE_PASSWORD_CHANGE, "User",
                                "Bulk force password change", httpRequest);
                        break;

                    case ASSIGN_ROLE:
                        if (request.getRoleId() == null) {
                            errors.add("Role ID is required for role assignment");
                            continue;
                        }
                        Role role = roleRepository.findById(request.getRoleId())
                                .orElseThrow(() -> new IllegalArgumentException("Role not found"));
                        user.getRoles().add(role);
                        logActivity(user.getId(), UserActivity.UserAction.ASSIGN_ROLE, "User",
                                "Bulk role assignment: " + role.getName(), httpRequest);
                        break;

                    case REMOVE_ROLE:
                        if (request.getRoleId() == null) {
                            errors.add("Role ID is required for role removal");
                            continue;
                        }
                        user.getRoles().removeIf(r -> r.getId().equals(request.getRoleId()));
                        logActivity(user.getId(), UserActivity.UserAction.REMOVE_ROLE, "User",
                                "Bulk role removal", httpRequest);
                        break;
                }
                successCount++;
            } catch (Exception e) {
                errors.add("Error processing user " + user.getEmail() + ": " + e.getMessage());
                log.error("Error in bulk operation for user: {}", user.getEmail(), e);
            }
        }

        userRepository.saveAll(users);

        result.put("successCount", successCount);
        result.put("errorCount", errors.size());
        result.put("errors", errors);

        log.info("Bulk operation completed. Success: {}, Errors: {}", successCount, errors.size());
        return result;
    }

    /**
     * Get user statistics
     */
    public Map<String, Object> getUserStatistics() {
        log.info("Generating user statistics");

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalUsers", userRepository.count());
        stats.put("activeUsers", userRepository.countByStatusAndIsDeletedFalse(User.UserStatus.ACTIVE));
        stats.put("inactiveUsers", userRepository.countByStatusAndIsDeletedFalse(User.UserStatus.INACTIVE));
        stats.put("lockedUsers", userRepository.countByStatusAndIsDeletedFalse(User.UserStatus.LOCKED));
        stats.put("pendingUsers", userRepository.countByStatusAndIsDeletedFalse(User.UserStatus.PENDING));
        stats.put("usersRequiringPasswordChange", userRepository.countByMustChangePasswordTrueAndIsDeletedFalse());

        LocalDateTime startOfMonth = LocalDateTime.now().withDayOfMonth(1).withHour(0).withMinute(0).withSecond(0);
        stats.put("newUsersThisMonth", userRepository.countByCreatedAtAfterAndIsDeletedFalse(startOfMonth));

        LocalDateTime startOfDay = LocalDateTime.now().withHour(0).withMinute(0).withSecond(0);
        stats.put("newUsersToday", userRepository.countByCreatedAtAfterAndIsDeletedFalse(startOfDay));

        return stats;
    }

    // Private helper methods

    private Specification<User> buildUserSpecification(UserSearchRequest request) {
        return (root, query, criteriaBuilder) -> {
            List<Predicate> predicates = new ArrayList<>();

            // Always exclude deleted users
            predicates.add(criteriaBuilder.isFalse(root.get("isDeleted")));

            // Text search
            if (request.getQuery() != null && !request.getQuery().trim().isEmpty()) {
                String searchTerm = "%" + request.getQuery().toLowerCase() + "%";
                Predicate namePredicate = criteriaBuilder.like(
                        criteriaBuilder.lower(root.get("name")), searchTerm);
                Predicate emailPredicate = criteriaBuilder.like(
                        criteriaBuilder.lower(root.get("email")), searchTerm);
                predicates.add(criteriaBuilder.or(namePredicate, emailPredicate));
            }

            // Status filter
            if (request.getStatuses() != null && !request.getStatuses().isEmpty()) {
                predicates.add(root.get("status").in(request.getStatuses()));
            }

            // Role filter
            if (request.getRoleIds() != null && !request.getRoleIds().isEmpty()) {
                predicates.add(root.join("roles").get("id").in(request.getRoleIds()));
            }

            // Date filters
            if (request.getCreatedAfter() != null) {
                predicates.add(criteriaBuilder.greaterThanOrEqualTo(root.get("createdAt"), request.getCreatedAfter()));
            }
            if (request.getCreatedBefore() != null) {
                predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("createdAt"), request.getCreatedBefore()));
            }
            if (request.getLastLoginAfter() != null) {
                predicates.add(
                        criteriaBuilder.greaterThanOrEqualTo(root.get("lastLoginAt"), request.getLastLoginAfter()));
            }
            if (request.getLastLoginBefore() != null) {
                predicates
                        .add(criteriaBuilder.lessThanOrEqualTo(root.get("lastLoginAt"), request.getLastLoginBefore()));
            }

            // Boolean filters
            if (request.getMustChangePassword() != null) {
                predicates.add(criteriaBuilder.equal(root.get("mustChangePassword"), request.getMustChangePassword()));
            }
            if (request.getIsActive() != null) {
                if (request.getIsActive()) {
                    predicates.add(criteriaBuilder.equal(root.get("status"), User.UserStatus.ACTIVE));
                } else {
                    predicates.add(criteriaBuilder.notEqual(root.get("status"), User.UserStatus.ACTIVE));
                }
            }

            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };
    }
}