package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.audit.queries.GetUserActivityQuery;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting user activity
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetUserActivityQueryHandler implements QueryHandler<GetUserActivityQuery, Page<GlobalAuditLog>> {

    private final GlobalAuditService globalAuditService;

    @Override
    public Page<GlobalAuditLog> handle(GetUserActivityQuery query) throws Exception {
        log.debug("Handling GetUserActivityQuery for user: {} in organization: {}", 
            query.getTargetUserId(), query.getOrganizationId());

        Page<GlobalAuditLog> activity = globalAuditService.getUserActivity(
            query.getOrganizationId(), 
            query.getTargetUserId(), 
            query.getPage(), 
            query.getSize()
        );
        
        return activity;
    }

    @Override
    public Class<GetUserActivityQuery> getQueryType() {
        return GetUserActivityQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetUserActivityQuery query) {
        return "audit-user:" + query.getOrganizationId() + ":" + 
               query.getTargetUserId() + ":" + query.getPage() + ":" + query.getSize();
    }
}