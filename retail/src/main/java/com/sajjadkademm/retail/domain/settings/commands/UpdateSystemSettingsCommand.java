package com.sajjadkademm.retail.domain.settings.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.settings.SystemSettingsRequest;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UpdateSystemSettingsCommand implements Command<SystemSetting> {
    private final String organizationId;
    private final SystemSettingsRequest request;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}