package com.sajjadkademm.retail.domain.settings.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.settings.queries.GetSystemSettingsQuery;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import com.sajjadkademm.retail.domain.settings.repositories.SystemSettingRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Query handler for getting system settings
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetSystemSettingsQueryHandler implements QueryHandler<GetSystemSettingsQuery, SystemSetting> {

    private final SystemSettingRepository systemSettingRepository;

    @Override
    public SystemSetting handle(GetSystemSettingsQuery query) throws Exception {
        log.debug("Handling GetSystemSettingsQuery for organization: {}", query.getOrganizationId());

        return systemSettingRepository.findByOrganizationId(query.getOrganizationId())
            .orElseThrow(() -> new NotFoundException("System settings not found"));
    }

    @Override
    public Class<GetSystemSettingsQuery> getQueryType() {
        return GetSystemSettingsQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    public String getCacheKey(GetSystemSettingsQuery query) {
        return "system-settings:" + query.getOrganizationId();
    }
}