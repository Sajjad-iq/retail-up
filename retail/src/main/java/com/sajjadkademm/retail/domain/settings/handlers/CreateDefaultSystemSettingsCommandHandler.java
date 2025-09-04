package com.sajjadkademm.retail.domain.settings.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.settings.commands.CreateDefaultSystemSettingsCommand;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;
import com.sajjadkademm.retail.domain.settings.repositories.SystemSettingRepository;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.enums.Currency;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for creating default system settings for new organizations
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CreateDefaultSystemSettingsCommandHandler implements CommandHandler<CreateDefaultSystemSettingsCommand, SystemSetting> {

    private final SystemSettingRepository systemSettingRepository;

    @Override
    public SystemSetting handle(CreateDefaultSystemSettingsCommand command) throws Exception {
        log.debug("Handling CreateDefaultSystemSettingsCommand for organization: {}", command.getOrganizationId());

        try {
            SystemSetting defaultSettings = SystemSetting.builder()
                .organizationId(command.getOrganizationId())
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

            SystemSetting savedSetting = systemSettingRepository.save(defaultSettings);

            log.info("Successfully created default system settings for organization: {} by user: {}", 
                command.getOrganizationId(), command.getUserId());

            return savedSetting;

        } catch (Exception e) {
            throw new BadRequestException("Failed to create default system settings: " + e.getMessage(), e);
        }
    }

    @Override
    public Class<CreateDefaultSystemSettingsCommand> getCommandType() {
        return CreateDefaultSystemSettingsCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}