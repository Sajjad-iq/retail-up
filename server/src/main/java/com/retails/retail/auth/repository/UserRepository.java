package com.retails.retail.auth.repository;

import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.Role;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * User repository for database operations
 * Provides enhanced query methods for user management
 */
@Repository
public interface UserRepository extends JpaRepository<User, UUID>, JpaSpecificationExecutor<User> {

        /**
         * Find user by email (case insensitive)
         */
        @Query("SELECT u FROM User u WHERE LOWER(u.email) = LOWER(:email) AND u.isDeleted = false")
        Optional<User> findByEmailIgnoreCase(@Param("email") String email);

        /**
         * Find user by employee ID
         */
        Optional<User> findByEmployeeIdAndIsDeletedFalse(String employeeId);

        /**
         * Find all active users
         */
        @Query("SELECT u FROM User u WHERE u.status = 'ACTIVE' AND u.isDeleted = false")
        List<User> findAllActiveUsers();

        /**
         * Find users by status
         */
        @Query("SELECT u FROM User u WHERE u.status = :status AND u.isDeleted = false")
        List<User> findByStatus(@Param("status") User.UserStatus status);

        /**
         * Find users by role ID
         */
        @Query("SELECT u FROM User u JOIN u.roles r WHERE r.id = :roleId AND u.isDeleted = false")
        List<User> findByRoleId(@Param("roleId") UUID roleId);

        /**
         * Find users by department
         */
        @Query("SELECT u FROM User u WHERE u.department = :department AND u.isDeleted = false")
        List<User> findByDepartment(@Param("department") String department);

        /**
         * Search users by name or email
         */
        @Query("SELECT u FROM User u WHERE " +
                        "(LOWER(u.name) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
                        "LOWER(u.email) LIKE LOWER(CONCAT('%', :query, '%'))) AND " +
                        "u.isDeleted = false")
        Page<User> searchUsers(@Param("query") String query, Pageable pageable);

        /**
         * Find users with filters
         */
        @Query("SELECT DISTINCT u FROM User u LEFT JOIN u.roles r WHERE " +
                        "(:query IS NULL OR LOWER(u.name) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
                        "LOWER(u.email) LIKE LOWER(CONCAT('%', :query, '%'))) AND " +
                        "(:roleId IS NULL OR r.id = :roleId) AND " +
                        "(:status IS NULL OR u.status = :status) AND " +
                        "(:department IS NULL OR u.department = :department) AND " +
                        "u.isDeleted = false")
        Page<User> findUsersWithFilters(
                        @Param("query") String query,
                        @Param("roleId") UUID roleId,
                        @Param("status") User.UserStatus status,
                        @Param("department") String department,
                        Pageable pageable);

        /**
         * Count users by status
         */
        @Query("SELECT COUNT(u) FROM User u WHERE u.status = :status AND u.isDeleted = false")
        long countByStatus(@Param("status") User.UserStatus status);

        /**
         * Count users created after date
         */
        @Query("SELECT COUNT(u) FROM User u WHERE u.createdAt >= :date AND u.isDeleted = false")
        long countUsersCreatedAfter(@Param("date") LocalDateTime date);

        /**
         * Find users requiring password change
         */
        @Query("SELECT u FROM User u WHERE u.mustChangePassword = true AND u.isDeleted = false")
        List<User> findUsersRequiringPasswordChange();

        /**
         * Check if email exists (excluding specific user ID for updates)
         */
        @Query("SELECT COUNT(u) > 0 FROM User u WHERE LOWER(u.email) = LOWER(:email) AND u.id != :excludeId AND u.isDeleted = false")
        boolean existsByEmailIgnoreCaseAndIdNot(@Param("email") String email, @Param("excludeId") UUID excludeId);

        /**
         * Check if employee ID exists (excluding specific user ID for updates)
         */
        @Query("SELECT COUNT(u) > 0 FROM User u WHERE u.employeeId = :employeeId AND u.id != :excludeId AND u.isDeleted = false")
        boolean existsByEmployeeIdAndIdNot(@Param("employeeId") String employeeId, @Param("excludeId") UUID excludeId);

        /**
         * Find users not logged in since given date
         */
        @Query("SELECT u FROM User u WHERE u.lastLoginAt < :date AND u.isDeleted = false")
        List<User> findUsersNotLoggedInSince(@Param("date") LocalDateTime date);

        /**
         * Count users by roles containing specific role and not deleted
         */
        @Query("SELECT COUNT(u) FROM User u JOIN u.roles r WHERE r = :role AND u.isDeleted = false")
        long countByRolesContainingAndIsDeletedFalse(@Param("role") Role role);

        /**
         * Find users by roles containing specific role and not deleted
         */
        @Query("SELECT u FROM User u JOIN u.roles r WHERE r = :role AND u.isDeleted = false")
        Page<User> findByRolesContainingAndIsDeletedFalse(@Param("role") Role role, Pageable pageable);

        /**
         * Check if email exists (case insensitive)
         */
        boolean existsByEmailIgnoreCase(String email);

        /**
         * Count by status and not deleted
         */
        long countByStatusAndIsDeletedFalse(User.UserStatus status);

        /**
         * Count by mustChangePassword and not deleted
         */
        long countByMustChangePasswordTrueAndIsDeletedFalse();

        /**
         * Count by createdAt after date and not deleted
         */
        long countByCreatedAtAfterAndIsDeletedFalse(LocalDateTime date);
}