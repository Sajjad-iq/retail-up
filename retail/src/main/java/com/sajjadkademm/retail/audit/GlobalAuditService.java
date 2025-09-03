package com.sajjadkademm.retail.audit;

import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.audit.enums.AuditAction;
import com.sajjadkademm.retail.audit.enums.EntityType;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.auth.model.User;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDateTime;
import java.util.List;

/**
 * GLOBAL AUDIT SERVICE: Simple and effective audit logging for the entire
 * organization
 * 
 * DESIGN PRINCIPLES:
 * - Simple API: Easy to use from any service
 * - Asynchronous: Don't slow down business operations
 * - Fail-safe: Audit failures don't break business logic
 * - Clean: Focused on essential audit functionality
 * 
 * USAGE PATTERNS:
 * 1. Inventory changes: auditInventoryChange()
 * 2. Entity changes: auditEntityChange()
 * 3. Business processes: auditBusinessProcess()
 * 4. Security events: auditSecurityEvent()
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GlobalAuditService {

    private final GlobalAuditRepository auditRepository;

    /**
     * INVENTORY AUDIT: Replace inventory movement logging
     * 
     * USE FOR: Stock changes, inventory adjustments, transfers
     * REPLACES: InventoryMovement entity logging
     */
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void auditInventoryChange(String organizationId, String itemId, String itemName,
            String movementType, Integer quantityChange,
            Integer stockBefore, Integer stockAfter,
            String reason, String referenceType, String referenceId,
            User user) {
        try {
            // Convert movement type to audit action
            AuditAction action = AuditAction.fromInventoryMovementType(movementType);

            // Build comprehensive description
            String description = buildInventoryDescription(movementType, quantityChange, reason);

            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(EntityType.INVENTORY_ITEM)
                    .entityId(itemId)
                    .entityName(itemName)
                    .action(action)
                    .description(description)
                    .quantityChange(quantityChange)
                    .quantityBefore(stockBefore)
                    .quantityAfter(stockAfter)
                    .businessProcess("Inventory Management")
                    .referenceType(referenceType)
                    .referenceId(referenceId)
                    .performedBy(user)
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(isLargeInventoryChange(quantityChange))
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {} (quantity: {})",
                    action, itemName, user.getEmail(), quantityChange);

        } catch (Exception e) {
            // FAIL-SAFE: Don't let audit failures break business logic
            log.error("Failed to log inventory audit for item {}: {}", itemId, e.getMessage(), e);
        }
    }

    /**
     * ENTITY AUDIT: General entity change tracking
     * 
     * USE FOR: User changes, organization updates, settings changes
     */
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void auditEntityChange(String organizationId, EntityType entityType, String entityId,
            String entityName, AuditAction action, String description,
            String fieldName, String oldValue, String newValue,
            User user) {
        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(entityType)
                    .entityId(entityId)
                    .entityName(entityName)
                    .action(action)
                    .description(description)
                    .fieldName(fieldName)
                    .oldValue(oldValue)
                    .newValue(newValue)
                    .businessProcess("Entity Management")
                    .performedBy(user)
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(action.isHighRisk() || entityType.isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {}", action, entityName, user.getEmail());

        } catch (Exception e) {
            log.error("Failed to log entity audit for {} {}: {}", entityType, entityId, e.getMessage(), e);
        }
    }

    /**
     * BUSINESS PROCESS AUDIT: Track important business workflows
     * 
     * USE FOR: Excel uploads, sales processes, bulk operations
     */
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void auditBusinessProcess(String organizationId, String processName, String description,
            String referenceType, String referenceId, User user) {
        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(EntityType.BUSINESS_PROCESS)
                    .entityId(referenceId)
                    .entityName(processName)
                    .action(AuditAction.PROCESS_EXECUTE)
                    .description(description)
                    .businessProcess(processName)
                    .referenceType(referenceType)
                    .referenceId(referenceId)
                    .performedBy(user)
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(false)
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: Process {} by {}", processName, user.getEmail());

        } catch (Exception e) {
            log.error("Failed to log business process audit for {}: {}", processName, e.getMessage(), e);
        }
    }

    /**
     * SECURITY AUDIT: Track security-related events
     * 
     * USE FOR: Login attempts, permission changes, suspicious activity
     */
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void auditSecurityEvent(String organizationId, AuditAction action, String description,
            String userId, String userName, String additionalInfo) {
        try {
            // For security events, user might be null (e.g., failed login)
            User currentUser = null;
            try {
                currentUser = SecurityUtils.getCurrentUser();
            } catch (Exception e) {
                // User not authenticated - that's okay for security events
            }

            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(organizationId)
                    .entityType(EntityType.USER)
                    .entityId(userId)
                    .entityName(userName)
                    .action(action)
                    .description(description)
                    .businessProcess("Security Management")
                    .oldValue(additionalInfo) // Store additional security context
                    .performedBy(currentUser) // May be null for failed logins
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(true) // All security events are sensitive
                    .build();

            auditRepository.save(auditLog);

            log.info("Security audit logged: {} for user {} from IP {}",
                    action, userName, getClientIp());

        } catch (Exception e) {
            log.error("Failed to log security audit for action {}: {}", action, e.getMessage(), e);
        }
    }

    /**
     * SIMPLE AUDIT: Quick logging for basic operations
     * 
     * USE FOR: Simple entity changes where you don't need all the parameters
     */
    public void auditSimple(String organizationId, EntityType entityType, String entityId,
            AuditAction action, String description, User user) {
        auditEntityChange(organizationId, entityType, entityId, null, action, description,
                null, null, null, user);
    }

    /**
     * QUERY METHODS: Get audit data for reports and analysis
     */

    // Get recent audit activity for dashboard
    public Page<GlobalAuditLog> getRecentActivity(String organizationId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return auditRepository.findByOrganizationIdOrderByCreatedAtDesc(organizationId, pageable);
    }

    // Get entity history
    public List<GlobalAuditLog> getEntityHistory(String organizationId, EntityType entityType, String entityId) {
        return auditRepository.findByOrganizationIdAndEntityTypeAndEntityIdOrderByCreatedAtDesc(
                organizationId, entityType, entityId);
    }

    // Get user activity
    public Page<GlobalAuditLog> getUserActivity(String organizationId, String userId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return auditRepository.findByOrganizationIdAndPerformedByIdOrderByCreatedAtDesc(
                organizationId, userId, pageable);
    }

    // Get inventory movements (replaces inventory movement queries)
    public Page<GlobalAuditLog> getInventoryMovements(String organizationId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return auditRepository.findInventoryMovements(organizationId, pageable);
    }

    // Search audit logs
    public Page<GlobalAuditLog> searchAuditLogs(String organizationId, String searchTerm, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return auditRepository.searchByDescription(organizationId, searchTerm, pageable);
    }

    /**
     * HELPER METHODS: Internal utilities
     */

    private String buildInventoryDescription(String movementType, Integer quantityChange, String reason) {
        StringBuilder desc = new StringBuilder();

        if (quantityChange != null) {
            if (quantityChange > 0) {
                desc.append("Added ").append(quantityChange).append(" units");
            } else {
                desc.append("Removed ").append(Math.abs(quantityChange)).append(" units");
            }
        }

        if (movementType != null) {
            desc.append(" via ").append(movementType.toLowerCase().replace("_", " "));
        }

        if (reason != null && !reason.trim().isEmpty()) {
            desc.append(" - ").append(reason);
        }

        return desc.toString();
    }

    private boolean isLargeInventoryChange(Integer quantityChange) {
        return quantityChange != null && Math.abs(quantityChange) > 100;
    }

    private String getClientIp() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                HttpServletRequest request = attributes.getRequest();
                String xForwardedFor = request.getHeader("X-Forwarded-For");
                if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
                    return xForwardedFor.split(",")[0].trim();
                }
                return request.getRemoteAddr();
            }
        } catch (Exception e) {
            // Ignore - audit context may not have request
        }
        return "unknown";
    }

    private String getUserAgent() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                return attributes.getRequest().getHeader("User-Agent");
            }
        } catch (Exception e) {
            // Ignore - audit context may not have request
        }
        return "unknown";
    }
}
