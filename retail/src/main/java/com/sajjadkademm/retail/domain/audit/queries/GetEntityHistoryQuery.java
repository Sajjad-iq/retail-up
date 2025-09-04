package com.sajjadkademm.retail.domain.audit.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class GetEntityHistoryQuery implements Query<List<GlobalAuditLog>> {
    private final String organizationId;
    private final EntityType entityType;
    private final String entityId;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}