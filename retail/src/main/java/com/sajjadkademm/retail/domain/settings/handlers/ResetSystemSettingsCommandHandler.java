package com.sajjadkademm.retail.domain.settings.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.settings.commands.ResetSystemSettingsCommand;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import com.sajjadkademm.retail.domain.settings.repositories.SystemSettingRepository;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidator;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;
import com.sajjadkademm.retail.shared.enums.Currency;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for resetting system settings to defaults
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ResetSystemSettingsCommandHandler implements CommandHandler<ResetSystemSettingsCommand, SystemSetting> {

    private final SystemSettingRepository systemSettingRepository;
    private final OrganizationRepository organizationRepository;
    private final OrganizationValidator organizationValidator;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public SystemSetting handle(ResetSystemSettingsCommand command) throws Exception {
        log.debug("Handling ResetSystemSettingsCommand for organization: {}", command.getOrganizationId());

        SystemSetting setting = systemSettingRepository.findByOrganizationId(command.getOrganizationId())
            .orElseThrow(() -> new NotFoundException("System settings not found"));

        Organization organization = organizationRepository.findById(command.getOrganizationId())
            .orElseThrow(() -> new NotFoundException("Organization not found"));

        // Validate organization is active
        organizationValidator.assertOrganizationIsActive(organization);

        // Create default settings
        SystemSetting defaultSettings = createDefaultSystemSettings(command.getOrganizationId());
        defaultSettings.setId(setting.getId());
        defaultSettings.setCreatedAt(setting.getCreatedAt());

        SystemSetting savedSetting = systemSettingRepository.save(defaultSettings);

        // Clear cache
        cacheInvalidationService.invalidateQueryCaches();

        log.info("Successfully reset system settings to defaults for organization: {} by user: {}", 
            command.getOrganizationId(), command.getUserId());

        return savedSetting;
    }

    private SystemSetting createDefaultSystemSettings(String organizationId) {
        return SystemSetting.builder()
            .organizationId(organizationId)
            // Backup Settings
            .autoBackupEnabled(true)
            .backupRetentionDays(30)
            // General Settings
            .timezone("UTC")
            .language("en")
            .currency(Currency.USD)
            // Notification Settings
            .emailNotificationsEnabled(true)
            .build();
    }

    @Override
    public Class<ResetSystemSettingsCommand> getCommandType() {
        return ResetSystemSettingsCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}