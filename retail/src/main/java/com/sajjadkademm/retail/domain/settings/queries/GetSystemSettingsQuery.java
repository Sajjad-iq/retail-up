package com.sajjadkademm.retail.domain.settings.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class GetSystemSettingsQuery implements Query<SystemSetting> {
    private final String organizationId;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}