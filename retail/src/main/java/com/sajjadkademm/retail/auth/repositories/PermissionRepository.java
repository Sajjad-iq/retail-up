package com.sajjadkademm.retail.auth.repositories;

import com.sajjadkademm.retail.auth.entities.Permission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for Permission entity.
 * Provides data access methods for permission management operations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Repository
public interface PermissionRepository extends JpaRepository<Permission, String> {

    /**
     * Find permission by name
     */
    Optional<Permission> findByName(String name);

    /**
     * Find permissions by category
     */
    List<Permission> findByCategory(Permission.PermissionCategory category);

    /**
     * Find system permissions
     */
    List<Permission> findByIsSystemTrue();

    /**
     * Find non-system permissions
     */
    List<Permission> findByIsSystemFalse();

    /**
     * Check if permission name exists
     */
    boolean existsByName(String name);

    /**
     * Find permissions by category and system flag
     */
    List<Permission> findByCategoryAndIsSystem(Permission.PermissionCategory category, Boolean isSystem);

    /**
     * Search permissions by name or label
     */
    @Query("SELECT p FROM Permission p WHERE " +
            "LOWER(p.name) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(p.label) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
            "LOWER(p.description) LIKE LOWER(CONCAT('%', :query, '%'))")
    List<Permission> searchPermissions(@Param("query") String query);

    /**
     * Find permissions not assigned to any user
     */
    @Query("SELECT p FROM Permission p WHERE p.id NOT IN " +
            "(SELECT DISTINCT perm.id FROM User u JOIN u.permissions perm)")
    List<Permission> findUnassignedPermissions();

    /**
     * Count permissions by category
     */
    long countByCategory(Permission.PermissionCategory category);

    /**
     * Count system permissions
     */
    long countByIsSystemTrue();

    /**
     * Find permissions used by specific user count
     */
    @Query("SELECT p.name, COUNT(u) FROM User u JOIN u.permissions p " +
            "WHERE p.id = :permissionId GROUP BY p.name")
    List<Object[]> countUsersWithPermission(@Param("permissionId") String permissionId);

    /**
     * Find most assigned permissions
     */
    @Query("SELECT p, COUNT(u) as userCount FROM User u JOIN u.permissions p " +
            "GROUP BY p ORDER BY userCount DESC")
    List<Object[]> findMostAssignedPermissions();

    /**
     * Find permissions by IDs
     */
    List<Permission> findByIdIn(List<String> ids);
}