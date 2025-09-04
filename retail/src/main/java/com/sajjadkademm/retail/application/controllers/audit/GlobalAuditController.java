package com.sajjadkademm.retail.application.controllers.audit;

import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.application.services.audit.AuditService;
import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.time.LocalDateTime;
import java.util.List;

import static com.sajjadkademm.retail.shared.constants.ValidationConstants.*;

/**
 * GLOBAL AUDIT CONTROLLER: Simple API for querying audit logs
 * 
 * ENDPOINTS:
 * - Get recent activity (dashboard)
 * - Get entity history
 * - Get user activity
 * - Get inventory movements
 * - Search audit logs
 * 
 * SECURITY: All endpoints are organization-scoped for data isolation
 */
@RestController
@RequestMapping("/api/v1/audit")
@RequiredArgsConstructor
@Tag(name = "Global Audit", description = "Organization-wide audit trail and activity tracking")
public class GlobalAuditController {

    private final AuditService auditService;

    /**
     * RECENT ACTIVITY: Get recent audit activity for dashboard
     */
    @GetMapping("/recent")
    @Operation(summary = "Get recent audit activity", description = "Retrieves recent audit activity for the organization dashboard")
    public ResponseEntity<Page<GlobalAuditLog>> getRecentActivity(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Page number (0-based)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "" + DEFAULT_PAGE_SIZE) int size) throws Exception {

        Page<GlobalAuditLog> activity = auditService.getRecentActivity(organizationId, page, size);
        return ResponseEntity.ok(activity);
    }

    /**
     * ENTITY HISTORY: Get complete change history for a specific entity
     */
    @GetMapping("/entity/{entityType}/{entityId}")
    @Operation(summary = "Get entity change history", description = "Retrieves complete audit history for a specific entity")
    public ResponseEntity<List<GlobalAuditLog>> getEntityHistory(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Entity type") @PathVariable EntityType entityType,
            @Parameter(description = "Entity ID") @PathVariable String entityId) throws Exception {

        List<GlobalAuditLog> history = auditService.getEntityHistory(organizationId, entityType, entityId);
        return ResponseEntity.ok(history);
    }

    /**
     * USER ACTIVITY: Get activity performed by a specific user
     */
    @GetMapping("/user/{userId}")
    @Operation(summary = "Get user activity", description = "Retrieves audit activity performed by a specific user")
    public ResponseEntity<Page<GlobalAuditLog>> getUserActivity(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "User ID") @PathVariable String userId,
            @Parameter(description = "Page number (0-based)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "" + DEFAULT_PAGE_SIZE) int size) throws Exception {

        Page<GlobalAuditLog> activity = auditService.getUserActivity(organizationId, userId, page, size);
        return ResponseEntity.ok(activity);
    }

    /**
     * INVENTORY MOVEMENTS: Get inventory-related audit logs (replaces old inventory
     * movement queries)
     */
    @GetMapping("/inventory-movements")
    @Operation(summary = "Get inventory movements", description = "Retrieves all inventory movement audit logs (stock changes)")
    public ResponseEntity<Page<GlobalAuditLog>> getInventoryMovements(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Page number (0-based)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "" + DEFAULT_PAGE_SIZE) int size) throws Exception {

        Page<GlobalAuditLog> movements = auditService.getInventoryMovements(organizationId, page, size);
        return ResponseEntity.ok(movements);
    }

    /**
     * SEARCH: Search audit logs by description or entity name
     */
    @GetMapping("/search")
    @Operation(summary = "Search audit logs", description = "Search audit logs by description text or entity name")
    public ResponseEntity<Page<GlobalAuditLog>> searchAuditLogs(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Search term") @RequestParam String searchTerm,
            @Parameter(description = "Page number (0-based)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size") @RequestParam(defaultValue = "" + DEFAULT_PAGE_SIZE) int size) throws Exception {

        Page<GlobalAuditLog> results = auditService.searchAuditLogs(organizationId, searchTerm, page, size);
        return ResponseEntity.ok(results);
    }

    /**
     * SECURITY DEMO: Log a security event (example of manual audit logging)
     */
    @PostMapping("/security-event")
    @Operation(summary = "Log security event", description = "Example endpoint showing how to manually log security events")
    public ResponseEntity<String> logSecurityEvent(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Event description") @RequestParam String description) throws Exception {

        String result = auditService.logSecurityEvent(organizationId, description);
        return ResponseEntity.ok(result);
    }

    /**
     * DEMO: Log a business process (example of process tracking)
     */
    @PostMapping("/business-process")
    @Operation(summary = "Log business process", description = "Example endpoint showing how to log business process execution")
    public ResponseEntity<String> logBusinessProcess(
            @Parameter(description = "Organization ID") @RequestParam String organizationId,
            @Parameter(description = "Process name") @RequestParam String processName,
            @Parameter(description = "Process description") @RequestParam String description,
            @Parameter(description = "Reference ID") @RequestParam String referenceId) throws Exception {

        String result = auditService.logBusinessProcess(organizationId, processName, description, referenceId);
        return ResponseEntity.ok(result);
    }
}
