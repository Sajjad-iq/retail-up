package com.sajjadkademm.retail.auth.repositories;

import com.sajjadkademm.retail.auth.entities.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository interface for User entity.
 * Provides data access methods for user management operations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Repository
public interface UserRepository extends JpaRepository<User, String> {

    /**
     * Find user by email address
     */
    Optional<User> findByEmail(String email);

    /**
     * Find user by employee ID
     */
    Optional<User> findByEmployeeId(String employeeId);

    /**
     * Check if email exists
     */
    boolean existsByEmail(String email);

    /**
     * Check if employee ID exists
     */
    boolean existsByEmployeeId(String employeeId);

    /**
     * Find users by status
     */
    List<User> findByStatus(User.UserStatus status);

    /**
     * Find users by department
     */
    List<User> findByDepartment(String department);

    /**
     * Find users by status and department
     */
    List<User> findByStatusAndDepartment(User.UserStatus status, String department);

    /**
     * Find users created after a specific date
     */
    List<User> findByCreatedAtAfter(LocalDateTime createdAt);

    /**
     * Find users who logged in after a specific date
     */
    List<User> findByLastLoginAtAfter(LocalDateTime lastLoginAt);

    /**
     * Search users by name or email with pagination
     */
    @Query("SELECT u FROM User u WHERE " +
            "LOWER(u.name) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(u.email) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(u.employeeId) LIKE LOWER(CONCAT('%', :query, '%'))")
    Page<User> searchUsers(@Param("query") String query, Pageable pageable);

    /**
     * Search users with filters
     */
    @Query("SELECT u FROM User u WHERE " +
            "(:query IS NULL OR " +
            "LOWER(u.name) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(u.email) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(u.employeeId) LIKE LOWER(CONCAT('%', :query, '%'))) AND " +
            "(:status IS NULL OR u.status = :status) AND " +
            "(:department IS NULL OR LOWER(u.department) = LOWER(:department))")
    Page<User> findUsersWithFilters(@Param("query") String query,
            @Param("status") User.UserStatus status,
            @Param("department") String department,
            Pageable pageable);

    /**
     * Find users who have specific permission
     */
    @Query("SELECT u FROM User u JOIN u.permissions p WHERE p.name = :permissionName")
    List<User> findUsersWithPermission(@Param("permissionName") String permissionName);

    /**
     * Find users in specific permission category
     */
    @Query("SELECT u FROM User u JOIN u.permissions p WHERE p.category = :category")
    List<User> findUsersWithPermissionCategory(
            @Param("category") com.sajjadkademm.retail.auth.entities.Permission.PermissionCategory category);

    /**
     * Count active users
     */
    long countByStatus(User.UserStatus status);

    /**
     * Count users by department
     */
    long countByDepartment(String department);

    /**
     * Count users created this month
     */
    @Query("SELECT COUNT(u) FROM User u WHERE u.createdAt >= :startOfMonth")
    long countUsersCreatedAfter(@Param("startOfMonth") LocalDateTime startOfMonth);

    /**
     * Count users who logged in today
     */
    @Query("SELECT COUNT(u) FROM User u WHERE u.lastLoginAt >= :startOfDay")
    long countUsersLoggedInAfter(@Param("startOfDay") LocalDateTime startOfDay);

    /**
     * Find most active users (based on last login)
     */
    @Query("SELECT u FROM User u WHERE u.lastLoginAt IS NOT NULL ORDER BY u.lastLoginAt DESC")
    List<User> findMostActiveUsers(Pageable pageable);

    /**
     * Get permission distribution count
     */
    @Query("SELECT p.name, COUNT(u) FROM User u JOIN u.permissions p GROUP BY p.name")
    List<Object[]> getPermissionDistribution();

    /**
     * Find users with multiple permissions
     */
    @Query("SELECT u FROM User u WHERE SIZE(u.permissions) > :minPermissions")
    List<User> findUsersWithMultiplePermissions(@Param("minPermissions") int minPermissions);

    /**
     * Find users who must change password
     */
    List<User> findByMustChangePasswordTrue();
}