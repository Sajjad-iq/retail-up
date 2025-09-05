package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.CreateInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import jakarta.servlet.http.HttpServletRequest;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for creating inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CreateInventoryCommandHandler implements CommandHandler<CreateInventoryCommand, Inventory> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditRepository auditRepository;
    private final UserRepository userRepository;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Inventory handle(CreateInventoryCommand command) throws Exception {
        log.debug("Handling CreateInventoryCommand for user: {}", command.getUserId());

        CreateInventoryRequest request = command.getRequest();
        
        // Validate request and access permissions
        validationUtils.validateCreateRequest(request, command.getUserId());
        
        // Get the current user for createdBy field
        User currentUser = userRepository.findById(command.getUserId())
                .orElseThrow(() -> new NotFoundException("User not found: " + command.getUserId()));
        
        // Build inventory from request
        Inventory inventory = Inventory.builder()
                .name(request.getName())
                .description(request.getDescription())
                .location(request.getLocation())
                .organizationId(request.getOrganizationId())
                .isActive(true)
                .createdBy(currentUser)
                .build();
        
        // Save inventory
        Inventory savedInventory = inventoryRepository.save(inventory);
        
        // Invalidate inventory-related caches
        cacheInvalidationService.invalidateInventoryCaches(
                savedInventory.getId(), 
                request.getOrganizationId()
        );
        
        // Log audit trail
        auditEntityChange(
                request.getOrganizationId(),
                EntityType.INVENTORY,
                savedInventory.getId(),
                savedInventory.getName(),
                AuditAction.CREATE,
                "Created inventory: " + savedInventory.getName(),
                null, // fieldName
                null, // oldValue
                null, // newValue
                savedInventory.getCreatedBy()
        );
        
        log.info("Successfully created inventory: {} for user: {}", 
                savedInventory.getId(), command.getUserId());
        
        return savedInventory;
    }

    @Override
    public Class<CreateInventoryCommand> getCommandType() {
        return CreateInventoryCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }

    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    private void auditEntityChange(String organizationId, EntityType entityType, String entityId,
            String entityName, AuditAction action, String description,
            String fieldName, String oldValue, String newValue, User user) {
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
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(action.isHighRisk() || entityType.isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {}", action, entityName, user.getEmail());

        } catch (Exception e) {
            log.error("Failed to log entity audit for {} {}: {}", entityType, entityId, e.getMessage(), e);
        }
    }

}