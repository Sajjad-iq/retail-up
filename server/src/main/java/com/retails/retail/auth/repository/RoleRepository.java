package com.retails.retail.auth.repository;

import com.retails.retail.auth.entity.Role;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Role repository for database operations
 * Provides enhanced query methods for role management
 */
@Repository
public interface RoleRepository extends JpaRepository<Role, UUID>, JpaSpecificationExecutor<Role> {

    /**
     * Find role by name (case insensitive)
     */
    @Query("SELECT r FROM Role r WHERE LOWER(r.name) = LOWER(:name) AND r.isDeleted = false")
    Optional<Role> findByNameIgnoreCase(@Param("name") String name);

    /**
     * Find all non-deleted roles
     */
    @Query("SELECT r FROM Role r WHERE r.isDeleted = false ORDER BY r.name")
    List<Role> findAllActive();

    /**
     * Find system roles
     */
    @Query("SELECT r FROM Role r WHERE r.isSystem = true AND r.isDeleted = false ORDER BY r.name")
    List<Role> findSystemRoles();

    /**
     * Find custom (non-system) roles
     */
    @Query("SELECT r FROM Role r WHERE r.isSystem = false AND r.isDeleted = false ORDER BY r.name")
    List<Role> findCustomRoles();

    /**
     * Check if role name exists (excluding specific role ID for updates)
     */
    @Query("SELECT COUNT(r) > 0 FROM Role r WHERE LOWER(r.name) = LOWER(:name) AND r.id != :excludeId AND r.isDeleted = false")
    boolean existsByNameIgnoreCaseAndIdNot(@Param("name") String name, @Param("excludeId") UUID excludeId);

    /**
     * Check if role name exists
     */
    @Query("SELECT COUNT(r) > 0 FROM Role r WHERE LOWER(r.name) = LOWER(:name) AND r.isDeleted = false")
    boolean existsByNameIgnoreCase(@Param("name") String name);

    /**
     * Count roles by system flag
     */
    @Query("SELECT COUNT(r) FROM Role r WHERE r.isSystem = :isSystem AND r.isDeleted = false")
    long countByIsSystem(@Param("isSystem") boolean isSystem);

    /**
     * Count all non-deleted roles
     */
    @Query("SELECT COUNT(r) FROM Role r WHERE r.isDeleted = false")
    long countByIsDeletedFalse();

    /**
     * Count system roles
     */
    @Query("SELECT COUNT(r) FROM Role r WHERE r.isSystem = true AND r.isDeleted = false")
    long countByIsSystemTrueAndIsDeletedFalse();

    /**
     * Count custom (non-system) roles
     */
    @Query("SELECT COUNT(r) FROM Role r WHERE r.isSystem = false AND r.isDeleted = false")
    long countByIsSystemFalseAndIsDeletedFalse();
}