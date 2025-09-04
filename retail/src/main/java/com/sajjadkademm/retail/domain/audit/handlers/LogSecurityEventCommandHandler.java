package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogSecurityEventCommand;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.user.model.User;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for logging security events
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LogSecurityEventCommandHandler implements CommandHandler<LogSecurityEventCommand, String> {

    private final GlobalAuditService globalAuditService;

    @Override
    public String handle(LogSecurityEventCommand command) throws Exception {
        log.debug("Handling LogSecurityEventCommand for organization: {}", command.getOrganizationId());

        User currentUser = SecurityUtils.getCurrentUser();

        globalAuditService.auditSecurityEvent(
            command.getOrganizationId(),
            AuditAction.SUSPICIOUS_ACTIVITY,
            command.getDescription(),
            currentUser.getId(),
            currentUser.getEmail(),
            "Manual security event logged via API"
        );

        log.info("Security event logged for organization: {} by user: {}", 
            command.getOrganizationId(), command.getUserId());

        return "Security event logged successfully";
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