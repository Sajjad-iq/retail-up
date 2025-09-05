package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogBusinessProcessCommand;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.utils.RequestContextUtils;

import org.springframework.stereotype.Component;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for logging business processes
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LogBusinessProcessCommandHandler implements CommandHandler<LogBusinessProcessCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogBusinessProcessCommand command) throws Exception {
        log.debug("Handling LogBusinessProcessCommand for process: {} in organization: {}", 
            command.getProcessName(), command.getOrganizationId());

        try {
            User currentUser = SecurityUtils.getCurrentUser();

            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(command.getOrganizationId())
                    .entityType(EntityType.BUSINESS_PROCESS)
                    .entityId(command.getReferenceId())
                    .entityName(command.getProcessName())
                    .action(AuditAction.PROCESS_EXECUTE)
                    .description(command.getDescription())
                    .businessProcess(command.getProcessName())
                    .referenceType("API_CALL")
                    .referenceId(command.getReferenceId())
                    .performedBy(currentUser)
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(false)
                    .build();

            auditRepository.save(auditLog);

            log.info("Business process logged for organization: {} by user: {}", 
                command.getOrganizationId(), command.getUserId());

            return "Business process logged successfully";

        } catch (Exception e) {
            log.error("Failed to log business process audit for {}: {}", command.getProcessName(), e.getMessage(), e);
            throw e;
        }
    }


    @Override
    public Class<LogBusinessProcessCommand> getCommandType() {
        return LogBusinessProcessCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}