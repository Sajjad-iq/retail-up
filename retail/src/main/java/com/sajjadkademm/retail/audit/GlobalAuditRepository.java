package com.sajjadkademm.retail.audit;

import com.sajjadkademm.retail.audit.enums.AuditAction;
import com.sajjadkademm.retail.audit.enums.EntityType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * GLOBAL AUDIT REPOSITORY: Simple and effective audit data access
 * 
 * DESIGN PRINCIPLES:
 * - Focused on common query patterns
 * - Performance-optimized with indexed fields
 * - Clean method names that express intent clearly
 */
@Repository
public interface GlobalAuditRepository extends JpaRepository<GlobalAuditLog, String> {

    /**
     * ORGANIZATION-SCOPED QUERIES: Most common access pattern
     * All queries should be scoped to organization for security and performance
     */

    // Get recent activity for an organization (dashboard view)
    Page<GlobalAuditLog> findByOrganizationIdOrderByCreatedAtDesc(String organizationId, Pageable pageable);

    // Get activity within a date range
    Page<GlobalAuditLog> findByOrganizationIdAndCreatedAtBetweenOrderByCreatedAtDesc(
            String organizationId, LocalDateTime from, LocalDateTime to, Pageable pageable);

    /**
     * ENTITY-SPECIFIC QUERIES: Track history of specific entities
     */

    // Get complete history of a specific entity
    List<GlobalAuditLog> findByOrganizationIdAndEntityTypeAndEntityIdOrderByCreatedAtDesc(
            String organizationId, EntityType entityType, String entityId);

    // Get recent changes to a specific entity
    Page<GlobalAuditLog> findByOrganizationIdAndEntityTypeAndEntityIdOrderByCreatedAtDesc(
            String organizationId, EntityType entityType, String entityId, Pageable pageable);

    /**
     * USER ACTIVITY QUERIES: Track what users are doing
     */

    // Get all activity by a specific user
    Page<GlobalAuditLog> findByOrganizationIdAndPerformedByIdOrderByCreatedAtDesc(
            String organizationId, String userId, Pageable pageable);

    // Get user activity within date range
    List<GlobalAuditLog> findByOrganizationIdAndPerformedByIdAndCreatedAtBetweenOrderByCreatedAtDesc(
            String organizationId, String userId, LocalDateTime from, LocalDateTime to);

    /**
     * ACTION-BASED QUERIES: Find specific types of activities
     */

    // Get all instances of a specific action
    Page<GlobalAuditLog> findByOrganizationIdAndActionOrderByCreatedAtDesc(
            String organizationId, AuditAction action, Pageable pageable);

    // Get inventory movements (stock changes)
    @Query("SELECT a FROM GlobalAuditLog a WHERE a.organizationId = :orgId " +
            "AND a.action IN ('STOCK_IN', 'STOCK_OUT', 'STOCK_ADJUST', 'STOCK_TRANSFER') " +
            "ORDER BY a.createdAt DESC")
    Page<GlobalAuditLog> findInventoryMovements(@Param("orgId") String organizationId, Pageable pageable);

    /**
     * SECURITY AND RISK QUERIES: Monitor sensitive activities
     */

    // Get high-risk activities (sensitive operations)
    List<GlobalAuditLog> findByOrganizationIdAndIsSensitiveTrueOrderByCreatedAtDesc(String organizationId);

    // Get failed login attempts
    List<GlobalAuditLog> findByOrganizationIdAndActionAndCreatedAtAfterOrderByCreatedAtDesc(
            String organizationId, AuditAction action, LocalDateTime since);

    /**
     * BUSINESS PROCESS QUERIES: Track workflows and processes
     */

    // Get all activities related to a business process
    List<GlobalAuditLog> findByOrganizationIdAndReferenceTypeAndReferenceIdOrderByCreatedAtDesc(
            String organizationId, String referenceType, String referenceId);

    // Get activities by business process name
    Page<GlobalAuditLog> findByOrganizationIdAndBusinessProcessOrderByCreatedAtDesc(
            String organizationId, String businessProcess, Pageable pageable);

    /**
     * SUMMARY AND ANALYTICS QUERIES: For reporting and dashboards
     */

    // Count activities by action type (for charts)
    @Query("SELECT a.action, COUNT(a) FROM GlobalAuditLog a " +
            "WHERE a.organizationId = :orgId " +
            "AND a.createdAt >= :since " +
            "GROUP BY a.action " +
            "ORDER BY COUNT(a) DESC")
    List<Object[]> countActivitiesByAction(@Param("orgId") String organizationId,
            @Param("since") LocalDateTime since);

    // Count activities by user (for user activity reports)
    @Query("SELECT a.performedBy.id, a.performedBy.name, a.performedBy.email, COUNT(a) " +
            "FROM GlobalAuditLog a " +
            "WHERE a.organizationId = :orgId " +
            "AND a.createdAt >= :since " +
            "GROUP BY a.performedBy.id, a.performedBy.name, a.performedBy.email " +
            "ORDER BY COUNT(a) DESC")
    List<Object[]> countActivitiesByUser(@Param("orgId") String organizationId,
            @Param("since") LocalDateTime since);

    // Count activities by entity type
    @Query("SELECT a.entityType, COUNT(a) FROM GlobalAuditLog a " +
            "WHERE a.organizationId = :orgId " +
            "AND a.createdAt >= :since " +
            "GROUP BY a.entityType " +
            "ORDER BY COUNT(a) DESC")
    List<Object[]> countActivitiesByEntityType(@Param("orgId") String organizationId,
            @Param("since") LocalDateTime since);

    /**
     * SEARCH QUERIES: For audit log search functionality
     */

    // Search by description text
    @Query("SELECT a FROM GlobalAuditLog a " +
            "WHERE a.organizationId = :orgId " +
            "AND (LOWER(a.description) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
            "OR LOWER(a.entityName) LIKE LOWER(CONCAT('%', :searchTerm, '%'))) " +
            "ORDER BY a.createdAt DESC")
    Page<GlobalAuditLog> searchByDescription(@Param("orgId") String organizationId,
            @Param("searchTerm") String searchTerm,
            Pageable pageable);

    /**
     * MAINTENANCE QUERIES: For system maintenance and cleanup
     */

    // Count total audit logs for an organization
    long countByOrganizationId(String organizationId);

    // Find old audit logs for cleanup (if implementing retention policy)
    @Query("SELECT a FROM GlobalAuditLog a " +
            "WHERE a.organizationId = :orgId " +
            "AND a.createdAt < :cutoffDate " +
            "AND a.isSensitive = false")
    List<GlobalAuditLog> findOldNonSensitiveAudits(@Param("orgId") String organizationId,
            @Param("cutoffDate") LocalDateTime cutoffDate);
}
