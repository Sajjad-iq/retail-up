package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.audit.queries.GetEntityHistoryQuery;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Query handler for getting entity change history
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetEntityHistoryQueryHandler implements QueryHandler<GetEntityHistoryQuery, List<GlobalAuditLog>> {

    private final GlobalAuditService globalAuditService;

    @Override
    public List<GlobalAuditLog> handle(GetEntityHistoryQuery query) throws Exception {
        log.debug("Handling GetEntityHistoryQuery for entity: {} - {}", 
            query.getEntityType(), query.getEntityId());

        List<GlobalAuditLog> history = globalAuditService.getEntityHistory(
            query.getOrganizationId(), 
            query.getEntityType(), 
            query.getEntityId()
        );
        
        return history;
    }

    @Override
    public Class<GetEntityHistoryQuery> getQueryType() {
        return GetEntityHistoryQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetEntityHistoryQuery query) {
        return "audit-entity:" + query.getOrganizationId() + ":" + 
               query.getEntityType() + ":" + query.getEntityId();
    }
}