package com.sajjadkademm.retail.domain.settings.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.settings.commands.UpdateSystemSettingsCommand;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import com.sajjadkademm.retail.domain.settings.repositories.SystemSettingRepository;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidator;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for updating system settings
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateSystemSettingsCommandHandler implements CommandHandler<UpdateSystemSettingsCommand, SystemSetting> {

    private final SystemSettingRepository systemSettingRepository;
    private final OrganizationRepository organizationRepository;
    private final OrganizationValidator organizationValidator;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public SystemSetting handle(UpdateSystemSettingsCommand command) throws Exception {
        log.debug("Handling UpdateSystemSettingsCommand for organization: {}", command.getOrganizationId());

        SystemSetting setting = systemSettingRepository.findByOrganizationId(command.getOrganizationId())
            .orElseThrow(() -> new NotFoundException("System settings not found"));

        Organization organization = organizationRepository.findById(command.getOrganizationId())
            .orElseThrow(() -> new NotFoundException("Organization not found"));

        // Validate organization is active
        organizationValidator.assertOrganizationIsActive(organization);

        // Apply updates
        var request = command.getRequest();
        setting.setBackupRetentionDays(request.getBackupRetentionDays());
        setting.setTimezone(request.getTimezone());
        setting.setLanguage(request.getLanguage());
        setting.setCurrency(request.getCurrency());
        setting.setEmailNotificationsEnabled(request.getEmailNotificationsEnabled());

        SystemSetting savedSetting = systemSettingRepository.save(setting);

        // Clear cache
        cacheInvalidationService.invalidateQueryCaches();

        log.info("Successfully updated system settings for organization: {} by user: {}", 
            command.getOrganizationId(), command.getUserId());

        return savedSetting;
    }

    @Override
    public Class<UpdateSystemSettingsCommand> getCommandType() {
        return UpdateSystemSettingsCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}