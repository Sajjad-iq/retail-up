package com.sajjadkademm.retail.domain.audit.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import org.springframework.data.domain.Page;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class GetRecentActivityQuery implements Query<Page<GlobalAuditLog>> {
    private final String organizationId;
    private final int page;
    private final int size;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}