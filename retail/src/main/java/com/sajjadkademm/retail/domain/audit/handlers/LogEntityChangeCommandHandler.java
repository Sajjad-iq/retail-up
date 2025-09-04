package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogEntityChangeCommand;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;

import org.springframework.stereotype.Component;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import jakarta.servlet.http.HttpServletRequest;

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
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
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