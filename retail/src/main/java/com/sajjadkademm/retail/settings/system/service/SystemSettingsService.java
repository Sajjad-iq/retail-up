package com.sajjadkademm.retail.settings.system.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.repository.SystemSettingRepository;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsResponse;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SystemSettingsService {

        private final SystemSettingRepository systemSettingRepository;

        /**
         * Get system settings for an organization
         */
        public SystemSettingsResponse getSystemSettings(String organizationId) {
                SystemSetting setting = systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found for organization: " + organizationId));

                return mapToResponse(setting);
        }

        /**
         * Update specific system settings
         */
        public SystemSettingsResponse updateSystemSettings(String organizationId, SystemSettingsRequest request,
                        String userId) {
                SystemSetting setting = systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found for organization: " + organizationId));

                setting.setTwoFactorAuthEnabled(request.getTwoFactorAuthEnabled());
                setting.setAutoBackupEnabled(request.getAutoBackupEnabled());
                setting.setBackupRetentionDays(request.getBackupRetentionDays());
                setting.setTimezone(request.getTimezone());
                setting.setLanguage(request.getLanguage());
                setting.setCurrency(request.getCurrency());
                setting.setEmailNotificationsEnabled(request.getEmailNotificationsEnabled());
                setting.setUpdatedBy(userId);
                SystemSetting updatedSetting = systemSettingRepository.save(setting);
                return mapToResponse(updatedSetting);
        }

        /**
         * Reset system settings to defaults
         */
        public SystemSettingsResponse resetToDefaults(String organizationId) {
                SystemSetting setting = systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found for organization: " + organizationId));

                SystemSetting defaultSettings = createDefaultSystemSettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                SystemSetting savedSetting = systemSettingRepository.save(defaultSettings);
                return mapToResponse(savedSetting);
        }

        /**
         * Create default system settings
         */
        public SystemSetting createDefaultSystemSettings(String organizationId) {
                return SystemSetting.builder()
                                .organizationId(organizationId)
                                .twoFactorAuthEnabled(false)
                                // Backup Settings
                                .autoBackupEnabled(true)
                                .backupRetentionDays(30)
                                // General Settings
                                .timezone("UTC")
                                .language("en")
                                .currency("USD")
                                // Notification Settings
                                .emailNotificationsEnabled(true)
                                .build();
        }

        /**
         * Create and save default system settings for a new organization
         */
        public SystemSetting createAndSaveDefaultSystemSettings(String organizationId, String createdBy) {
                try {
                        SystemSetting defaultSettings = createDefaultSystemSettings(organizationId);
                        defaultSettings.setUpdatedBy(createdBy);
                        SystemSetting savedSettings = systemSettingRepository.save(defaultSettings);
                        return savedSettings;
                } catch (Exception e) {
                        throw new BadRequestException("Failed to create default system settings: " + e.getMessage(), e);
                }
        }

        /**
         * Map SystemSetting entity to SystemSettingsResponse DTO
         */
        private SystemSettingsResponse mapToResponse(SystemSetting setting) {
                return SystemSettingsResponse.builder()
                                .id(setting.getId())
                                .organizationId(setting.getOrganizationId())
                                .twoFactorAuthEnabled(setting.getTwoFactorAuthEnabled())
                                // Backup Settings
                                .autoBackupEnabled(setting.getAutoBackupEnabled())
                                .backupRetentionDays(setting.getBackupRetentionDays())
                                // General Settings
                                .timezone(setting.getTimezone())
                                .language(setting.getLanguage())
                                .currency(setting.getCurrency())
                                // Notification Settings
                                .emailNotificationsEnabled(setting.getEmailNotificationsEnabled())
                                // Audit Fields
                                .updatedBy(setting.getUpdatedBy())
                                .createdAt(setting.getCreatedAt())
                                .updatedAt(setting.getUpdatedAt())
                                .build();
        }
}