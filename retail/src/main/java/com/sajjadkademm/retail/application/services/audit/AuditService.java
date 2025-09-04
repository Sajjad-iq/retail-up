package com.sajjadkademm.retail.application.services.audit;

import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.audit.queries.*;
import com.sajjadkademm.retail.domain.audit.commands.*;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.user.model.User;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Service class for audit operations.
 * Handles business logic for organization-wide audit trail and activity tracking.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AuditService {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Get recent audit activity for dashboard
     * 
     * @param organizationId Organization ID
     * @param page Page number (0-based)
     * @param size Page size
     * @return Page of recent audit activity
     * @throws Exception if retrieval fails
     */
    public Page<GlobalAuditLog> getRecentActivity(String organizationId, int page, int size) throws Exception {
        log.debug("Getting recent activity for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        GetRecentActivityQuery query = GetRecentActivityQuery.builder()
                .organizationId(organizationId)
                .page(page)
                .size(size)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get complete change history for a specific entity
     * 
     * @param organizationId Organization ID
     * @param entityType Entity type
     * @param entityId Entity ID
     * @return List of audit history for the entity
     * @throws Exception if retrieval fails
     */
    public List<GlobalAuditLog> getEntityHistory(String organizationId, EntityType entityType, String entityId) throws Exception {
        log.debug("Getting entity history for {}/{} in organization: {} by user: {}", 
                entityType, entityId, organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        GetEntityHistoryQuery query = GetEntityHistoryQuery.builder()
                .organizationId(organizationId)
                .entityType(entityType)
                .entityId(entityId)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get activity performed by a specific user
     * 
     * @param organizationId Organization ID
     * @param userId Target user ID
     * @param page Page number (0-based)
     * @param size Page size
     * @return Page of user activity
     * @throws Exception if retrieval fails
     */
    public Page<GlobalAuditLog> getUserActivity(String organizationId, String userId, int page, int size) throws Exception {
        log.debug("Getting user activity for user: {} in organization: {} by user: {}", 
                userId, organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        GetUserActivityQuery query = GetUserActivityQuery.builder()
                .organizationId(organizationId)
                .targetUserId(userId)
                .page(page)
                .size(size)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get inventory-related audit logs (inventory movements)
     * 
     * @param organizationId Organization ID
     * @param page Page number (0-based)
     * @param size Page size
     * @return Page of inventory movement audit logs
     * @throws Exception if retrieval fails
     */
    public Page<GlobalAuditLog> getInventoryMovements(String organizationId, int page, int size) throws Exception {
        log.debug("Getting inventory movements for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        GetInventoryMovementsQuery query = GetInventoryMovementsQuery.builder()
                .organizationId(organizationId)
                .page(page)
                .size(size)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Search audit logs by description text or entity name
     * 
     * @param organizationId Organization ID
     * @param searchTerm Search term
     * @param page Page number (0-based)
     * @param size Page size
     * @return Page of matching audit logs
     * @throws Exception if search fails
     */
    public Page<GlobalAuditLog> searchAuditLogs(String organizationId, String searchTerm, int page, int size) throws Exception {
        log.debug("Searching audit logs in organization: {} with term: {} by user: {}", 
                organizationId, searchTerm, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        SearchAuditLogsQuery query = SearchAuditLogsQuery.builder()
                .organizationId(organizationId)
                .searchTerm(searchTerm)
                .page(page)
                .size(size)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Log a security event (manual audit logging example)
     * 
     * @param organizationId Organization ID
     * @param description Event description
     * @return Result message
     * @throws Exception if logging fails
     */
    public String logSecurityEvent(String organizationId, String description) throws Exception {
        log.debug("Logging security event for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        LogSecurityEventCommand command = LogSecurityEventCommand.builder()
                .organizationId(organizationId)
                .description(description)
                .userId(currentUser.getId())
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Log a business process (process tracking example)
     * 
     * @param organizationId Organization ID
     * @param processName Process name
     * @param description Process description
     * @param referenceId Reference ID
     * @return Result message
     * @throws Exception if logging fails
     */
    public String logBusinessProcess(String organizationId, String processName, String description, String referenceId) throws Exception {
        log.debug("Logging business process {} for organization: {} by user: {}", 
                processName, organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        LogBusinessProcessCommand command = LogBusinessProcessCommand.builder()
                .organizationId(organizationId)
                .processName(processName)
                .description(description)
                .referenceId(referenceId)
                .userId(currentUser.getId())
                .build();
        
        return commandBus.execute(command);
    }
}