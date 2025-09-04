package com.sajjadkademm.retail.domain.audit.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class LogSimpleAuditCommand implements Command<String> {
    private final String organizationId;
    private final EntityType entityType;
    private final String entityId;
    private final AuditAction action;
    private final String description;
    private final User user;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}