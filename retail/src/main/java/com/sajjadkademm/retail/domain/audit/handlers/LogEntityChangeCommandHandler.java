package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogEntityChangeCommand;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
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
public class LogEntityChangeCommandHandler implements CommandHandler<LogEntityChangeCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogEntityChangeCommand command) throws Exception {
        log.debug("Handling LogEntityChangeCommand for entity: {} in organization: {}", 
            command.getEntityName(), command.getOrganizationId());

        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(command.getOrganizationId())
                    .entityType(command.getEntityType())
                    .entityId(command.getEntityId())
                    .entityName(command.getEntityName())
                    .action(command.getAction())
                    .description(command.getDescription())
                    .fieldName(command.getFieldName())
                    .oldValue(command.getOldValue())
                    .newValue(command.getNewValue())
                    .businessProcess("Entity Management")
                    .performedBy(command.getUser())
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(command.getAction().isHighRisk() || command.getEntityType().isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Audit logged: {} {} by {}", command.getAction(), command.getEntityName(), command.getUser().getEmail());

            return "Entity change logged successfully";

        } catch (Exception e) {
            log.error("Failed to log entity audit for {} {}: {}", command.getEntityType(), command.getEntityId(), e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public Class<LogEntityChangeCommand> getCommandType() {
        return LogEntityChangeCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }

}