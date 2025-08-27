package com.sajjadkademm.retail.settings.system.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.repository.SystemSettingRepository;
import com.sajjadkademm.retail.shared.enums.Currency;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationRepository;
import com.sajjadkademm.retail.organizations.validator.OrganizationValidationUtils;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SystemSettingsService {

        private final SystemSettingRepository systemSettingRepository;
        private final OrganizationRepository organizationRepository;
        private final OrganizationValidationUtils organizationValidationUtils;

        /**
         * Get system settings for an organization
         */
        public SystemSetting getSystemSettings(String organizationId) {
                return systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found"));
        }

        /**
         * Update specific system settings
         */
        public SystemSetting updateSystemSettings(String organizationId, SystemSettingsRequest request) {
                SystemSetting setting = systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found"));

                Organization organization = organizationRepository.findById(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Organization not found"));

                // assert organization is active
                organizationValidationUtils.assertOrganizationIsActive(organization);

                setting.setBackupRetentionDays(request.getBackupRetentionDays());
                setting.setTimezone(request.getTimezone());
                setting.setLanguage(request.getLanguage());
                setting.setCurrency(request.getCurrency());
                setting.setEmailNotificationsEnabled(request.getEmailNotificationsEnabled());

                return systemSettingRepository.save(setting);
        }

        /**
         * Reset system settings to defaults
         */
        public SystemSetting resetToDefaults(String organizationId) {
                SystemSetting setting = systemSettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "System settings not found"));

                Organization organization = organizationRepository.findById(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Organization not found"));

                // assert organization is active
                organizationValidationUtils.assertOrganizationIsActive(organization);

                SystemSetting defaultSettings = createDefaultSystemSettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                return systemSettingRepository.save(defaultSettings);
        }

        /**
         * Create default system settings
         */
        public SystemSetting createDefaultSystemSettings(String organizationId) {
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

        /**
         * Create and save default system settings for a new organization
         */
        public SystemSetting createAndSaveDefaultSystemSettings(String organizationId, String createdBy) {
                try {
                        SystemSetting defaultSettings = createDefaultSystemSettings(organizationId);
                        return systemSettingRepository.save(defaultSettings);
                } catch (Exception e) {
                        throw new BadRequestException("Failed to create default system settings: " + e.getMessage(), e);
                }
        }
}