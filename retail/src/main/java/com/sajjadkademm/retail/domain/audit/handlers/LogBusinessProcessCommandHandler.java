package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.audit.commands.LogBusinessProcessCommand;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.auth.model.User;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for logging business processes
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class LogBusinessProcessCommandHandler implements CommandHandler<LogBusinessProcessCommand, String> {

    private final GlobalAuditService globalAuditService;

    @Override
    public String handle(LogBusinessProcessCommand command) throws Exception {
        log.debug("Handling LogBusinessProcessCommand for process: {} in organization: {}", 
            command.getProcessName(), command.getOrganizationId());

        User currentUser = SecurityUtils.getCurrentUser();

        globalAuditService.auditBusinessProcess(
            command.getOrganizationId(),
            command.getProcessName(),
            command.getDescription(),
            "API_CALL",
            command.getReferenceId(),
            currentUser
        );

        log.info("Business process logged for organization: {} by user: {}", 
            command.getOrganizationId(), command.getUserId());

        return "Business process logged successfully";
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