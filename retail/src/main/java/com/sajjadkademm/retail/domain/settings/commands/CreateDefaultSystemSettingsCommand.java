package com.sajjadkademm.retail.domain.settings.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CreateDefaultSystemSettingsCommand implements Command<SystemSetting> {
    private final String organizationId;
    private final String createdBy;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}