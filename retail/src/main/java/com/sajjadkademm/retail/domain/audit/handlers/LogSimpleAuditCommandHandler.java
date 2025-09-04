package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogSimpleAuditCommand;
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
public class LogSimpleAuditCommandHandler implements CommandHandler<LogSimpleAuditCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogSimpleAuditCommand command) throws Exception {
        log.debug("Handling LogSimpleAuditCommand for entity: {} in organization: {}", 
            command.getEntityId(), command.getOrganizationId());

        try {
            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(command.getOrganizationId())
                    .entityType(command.getEntityType())
                    .entityId(command.getEntityId())
                    .entityName(null) // Not provided in simple audit
                    .action(command.getAction())
                    .description(command.getDescription())
                    .fieldName(null)
                    .oldValue(null)
                    .newValue(null)
                    .businessProcess("Simple Entity Management")
                    .performedBy(command.getUser())
                    .sourceIp(getClientIp())
                    .userAgent(getUserAgent())
                    .isSensitive(command.getAction().isHighRisk() || command.getEntityType().isSensitiveByDefault())
                    .build();

            auditRepository.save(auditLog);

            log.debug("Simple audit logged: {} {} by {}", command.getAction(), command.getEntityId(), command.getUser().getEmail());

            return "Simple audit logged successfully";

        } catch (Exception e) {
            log.error("Failed to log simple audit for {} {}: {}", command.getEntityType(), command.getEntityId(), e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public Class<LogSimpleAuditCommand> getCommandType() {
        return LogSimpleAuditCommand.class;
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