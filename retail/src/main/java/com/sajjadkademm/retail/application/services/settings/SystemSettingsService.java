package com.sajjadkademm.retail.application.services.settings;

import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.settings.queries.GetSystemSettingsQuery;
import com.sajjadkademm.retail.domain.settings.commands.*;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.application.dto.settings.SystemSettingsRequest;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Service class for system settings operations.
 * Handles business logic for managing organization-wide system configurations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SystemSettingsService {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Get system settings for an organization
     * 
     * @param organizationId Organization ID
     * @return System settings for the organization
     * @throws Exception if retrieval fails
     */
    public SystemSetting getSystemSettings(String organizationId) throws Exception {
        log.debug("Getting system settings for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        GetSystemSettingsQuery query = GetSystemSettingsQuery.builder()
                .organizationId(organizationId)
                .userId(currentUser.getId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Update system settings for an organization
     * 
     * @param organizationId Organization ID
     * @param request System settings update request
     * @return Updated system settings
     * @throws Exception if update fails
     */
    public SystemSetting updateSystemSettings(String organizationId, SystemSettingsRequest request) throws Exception {
        log.debug("Updating system settings for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        UpdateSystemSettingsCommand command = UpdateSystemSettingsCommand.builder()
                .organizationId(organizationId)
                .request(request)
                .userId(currentUser.getId())
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Reset system settings to defaults for an organization
     * 
     * @param organizationId Organization ID
     * @return Reset system settings with default values
     * @throws Exception if reset fails
     */
    public SystemSetting resetSystemSettingsToDefaults(String organizationId) throws Exception {
        log.debug("Resetting system settings to defaults for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        User currentUser = SecurityUtils.getCurrentUser();
        
        ResetSystemSettingsCommand command = ResetSystemSettingsCommand.builder()
                .organizationId(organizationId)
                .userId(currentUser.getId())
                .build();
        
        return commandBus.execute(command);
    }
}