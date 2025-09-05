package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogSecurityAuditCommand;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;
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

@Slf4j
@Component
@RequiredArgsConstructor
public class LogSecurityAuditCommandHandler implements CommandHandler<LogSecurityAuditCommand, String> {

    private final GlobalAuditRepository auditRepository;

    @Override
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public String handle(LogSecurityAuditCommand command) throws Exception {
        log.debug("Handling LogSecurityAuditCommand for action: {}", command.getAction());

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
                    .entityId(command.getTargetUserId())
                    .entityName(command.getTargetUserName())
                    .action(command.getAction())
                    .description(command.getDescription())
                    .businessProcess("Security Management")
                    .oldValue(command.getAdditionalInfo()) // Store additional security context
                    .performedBy(currentUser) // May be null for failed logins
                    .sourceIp(command.getClientIp())
                    .userAgent(command.getUserAgent())
                    .isSensitive(true) // All security events are sensitive
                    .build();

            auditRepository.save(auditLog);

            log.info("Security audit logged: {} for user {} from IP {}",
                    command.getAction(), command.getTargetUserName(), command.getClientIp());

            return "Security audit logged successfully";

        } catch (Exception e) {
            log.error("Failed to log security audit for action {}: {}", command.getAction(), e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public Class<LogSecurityAuditCommand> getCommandType() {
        return LogSecurityAuditCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}