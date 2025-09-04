package com.sajjadkademm.retail.domain.audit.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class LogBusinessProcessCommand implements Command<String> {
    private final String organizationId;
    private final String processName;
    private final String description;
    private final String referenceId;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}