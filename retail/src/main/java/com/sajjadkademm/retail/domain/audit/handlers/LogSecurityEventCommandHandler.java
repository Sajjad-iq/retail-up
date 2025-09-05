package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogSecurityEventCommand;
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
 * Command handler for logging security events
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LogSecurityEventCommandHandler implements CommandHandler<LogSecurityEventCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogSecurityEventCommand command) throws Exception {
        log.debug("Handling LogSecurityEventCommand for organization: {}", command.getOrganizationId());

        try {
            // For security events, user might be null (e.g., failed login)
            User currentUser = null;
            try {
                currentUser = SecurityUtils.getCurrentUser();
            } catch (Exception e) {
                // User not authenticated - that's okay for security events
            }

            GlobalAuditLog auditLog = GlobalAuditLog.builder()
                    .organizationId(command.getOrganizationId())
                    .entityType(EntityType.USER)
                    .entityId(currentUser != null ? currentUser.getId() : null)
                    .entityName(currentUser != null ? currentUser.getEmail() : "Unknown")
                    .action(AuditAction.SUSPICIOUS_ACTIVITY)
                    .description(command.getDescription())
                    .businessProcess("Security Management")
                    .oldValue("Manual security event logged via API") // Store additional security context
                    .performedBy(currentUser) // May be null for failed logins
                    .sourceIp(RequestContextUtils.getClientIp())
                    .userAgent(RequestContextUtils.getUserAgent())
                    .isSensitive(true) // All security events are sensitive
                    .build();

            auditRepository.save(auditLog);

            log.info("Security event logged for organization: {} by user: {}", 
                command.getOrganizationId(), command.getUserId());

            return "Security event logged successfully";

        } catch (Exception e) {
            log.error("Failed to log security audit for action {}: {}", AuditAction.SUSPICIOUS_ACTIVITY, e.getMessage(), e);
            throw e;
        }
    }


    @Override
    public Class<LogSecurityEventCommand> getCommandType() {
        return LogSecurityEventCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}