package com.sajjadkademm.retail.domain.audit.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class LogSecurityAuditCommand implements Command<String> {
    private final String organizationId;
    private final AuditAction action;
    private final String description;
    private final String targetUserId;
    private final String targetUserName;
    private final String additionalInfo;
    private final String clientIp;
    private final String userAgent;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}