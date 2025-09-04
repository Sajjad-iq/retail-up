package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.audit.queries.GetRecentActivityQuery;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting recent audit activity
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetRecentActivityQueryHandler implements QueryHandler<GetRecentActivityQuery, Page<GlobalAuditLog>> {

    private final GlobalAuditRepository auditRepository;

    @Override
    public Page<GlobalAuditLog> handle(GetRecentActivityQuery query) throws Exception {
        log.debug("Handling GetRecentActivityQuery for organization: {}", query.getOrganizationId());

        // TODO: Add organization access validation
        Page<GlobalAuditLog> activity = auditRepository.findByOrganizationIdOrderByCreatedAtDesc(
            query.getOrganizationId(), 
            PageRequest.of(query.getPage(), query.getSize())
        );
        
        return activity;
    }

    @Override
    public Class<GetRecentActivityQuery> getQueryType() {
        return GetRecentActivityQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetRecentActivityQuery query) {
        return "audit-recent:" + query.getOrganizationId() + ":" + 
               query.getPage() + ":" + query.getSize();
    }
}