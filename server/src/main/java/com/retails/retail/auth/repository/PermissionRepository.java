package com.retails.retail.auth.repository;

import com.retails.retail.auth.entity.Permission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

/**
 * Permission repository for database operations
 * Provides enhanced query methods for permission management
 */
@Repository
public interface PermissionRepository extends JpaRepository<Permission, UUID> {

    /**
     * Find permission by name
     */
    Optional<Permission> findByName(String name);

    /**
     * Find permissions by category
     */
    @Query("SELECT p FROM Permission p WHERE p.category = :category ORDER BY p.label")
    List<Permission> findByCategory(@Param("category") Permission.PermissionCategory category);

    /**
     * Find permissions by IDs
     */
    @Query("SELECT p FROM Permission p WHERE p.id IN :ids")
    Set<Permission> findByIdIn(@Param("ids") List<UUID> ids);

    /**
     * Find all permissions ordered by category and label
     */
    @Query("SELECT p FROM Permission p ORDER BY p.category, p.label")
    List<Permission> findAllOrderedByCategoryAndLabel();

    /**
     * Find system permissions
     */
    @Query("SELECT p FROM Permission p WHERE p.isSystem = true ORDER BY p.category, p.label")
    List<Permission> findSystemPermissions();

    /**
     * Find custom permissions
     */
    @Query("SELECT p FROM Permission p WHERE p.isSystem = false ORDER BY p.category, p.label")
    List<Permission> findCustomPermissions();

    /**
     * Check if permission name exists
     */
    boolean existsByName(String name);

    /**
     * Count permissions by category
     */
    @Query("SELECT COUNT(p) FROM Permission p WHERE p.category = :category")
    long countByCategory(@Param("category") Permission.PermissionCategory category);

    /**
     * Get permissions grouped by category
     */
    @Query("SELECT p FROM Permission p ORDER BY p.category, p.label")
    List<Permission> findAllGroupedByCategory();
}