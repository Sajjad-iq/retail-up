package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogInventoryChangeCommand;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import org.springframework.stereotype.Component;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;


@Slf4j
@Component
@RequiredArgsConstructor
public class LogInventoryChangeCommandHandler implements CommandHandler<LogInventoryChangeCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogInventoryChangeCommand command) throws Exception {
        log.debug("Handling LogInventoryChangeCommand for item: {} in organization: {}", 
            command.getItemName(), command.getOrganizationId());

        try {
            // Convert movement type to audit action
            AuditAction action = AuditAction.fromInventoryMovementType(command.getMovementType());

            // Build comprehensive description
            String description = buildInventoryDescription(command.getMovementType(), command.getQuantityChange(), command.getReason());

            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(command.getOrganizationId())
                    .entityType(EntityType.INVENTORY_ITEM)
                    .entityId(command.getItemId())
                    .entityName(command.getItemName())
                    .action(action)
                    .description(description)
                    .quantityChange(command.getQuantityChange())
                    .quantityBefore(command.getStockBefore())
                    .quantityAfter(command.getStockAfter())
                    .businessProcess("Inventory Management")
                    .referenceType(command.getReferenceType())
                    .referenceId(command.getReferenceId())
                    .performedBy(command.getUser())
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(isLargeInventoryChange(command.getQuantityChange()))
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {} (quantity: {})",
                    action, command.getItemName(), command.getUser().getEmail(), command.getQuantityChange());

            return "Inventory change logged successfully";

        } catch (Exception e) {
            // FAIL-SAFE: Don't let audit failures break business logic
            log.error("Failed to log inventory audit for item {}: {}", command.getItemId(), e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public Class<LogInventoryChangeCommand> getCommandType() {
        return LogInventoryChangeCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }

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
        return quantityChange != null && Math.abs(quantityChange) > ValidationConstants.AUDIT_SIGNIFICANT_CHANGE_THRESHOLD;
    }

}