package com.sajjadkademm.retail.domain.audit.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.audit.queries.GetInventoryMovementsQuery;
import com.sajjadkademm.retail.domain.audit.model.GlobalAuditLog;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting inventory movements
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventoryMovementsQueryHandler implements QueryHandler<GetInventoryMovementsQuery, Page<GlobalAuditLog>> {

    private final GlobalAuditService globalAuditService;

    @Override
    public Page<GlobalAuditLog> handle(GetInventoryMovementsQuery query) throws Exception {
        log.debug("Handling GetInventoryMovementsQuery for organization: {}", query.getOrganizationId());

        Page<GlobalAuditLog> movements = globalAuditService.getInventoryMovements(
            query.getOrganizationId(), 
            query.getPage(), 
            query.getSize()
        );
        
        return movements;
    }

    @Override
    public Class<GetInventoryMovementsQuery> getQueryType() {
        return GetInventoryMovementsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetInventoryMovementsQuery query) {
        return "audit-inventory-movements:" + query.getOrganizationId() + ":" + 
               query.getPage() + ":" + query.getSize();
    }
}