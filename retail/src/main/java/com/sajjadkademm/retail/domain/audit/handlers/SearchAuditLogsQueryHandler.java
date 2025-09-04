package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.audit.queries.SearchAuditLogsQuery;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.domain.audit.repositories.GlobalAuditRepository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for searching audit logs
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class SearchAuditLogsQueryHandler implements QueryHandler<SearchAuditLogsQuery, Page<GlobalAuditLog>> {

    private final GlobalAuditRepository auditRepository;

    @Override
    public Page<GlobalAuditLog> handle(SearchAuditLogsQuery query) throws Exception {
        log.debug("Handling SearchAuditLogsQuery for organization: {} with term: {}", 
            query.getOrganizationId(), query.getSearchTerm());

        Page<GlobalAuditLog> results = auditRepository.searchByDescription(
            query.getOrganizationId(), 
            query.getSearchTerm(), 
            PageRequest.of(query.getPage(), query.getSize())
        );
        
        return results;
    }

    @Override
    public Class<SearchAuditLogsQuery> getQueryType() {
        return SearchAuditLogsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(SearchAuditLogsQuery query) {
        return "audit-search:" + query.getOrganizationId() + ":" + 
               query.getSearchTerm().hashCode() + ":" + query.getPage() + ":" + query.getSize();
    }
}