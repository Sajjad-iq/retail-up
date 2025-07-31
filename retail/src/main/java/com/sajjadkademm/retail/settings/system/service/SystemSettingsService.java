package com.sajjadkademm.retail.settings.system.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.repository.SystemSettingRepository;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class SystemSettingsService {

    private final SystemSettingRepository systemSettingRepository;

    /**
     * Get all system settings for an organization
     */
    public List<SystemSettingsResponse> getSystemSettings(String organizationId) {
        return systemSettingRepository.findByOrganizationId(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get system setting by key
     */
    public SystemSettingsResponse getSystemSettingByKey(String organizationId, String key) {
        SystemSetting setting = systemSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "System setting not found for organization: " + organizationId + ", key: " + key));

        return mapToResponse(setting);
    }

    /**
     * Create or update system setting
     */
    public SystemSettingsResponse createOrUpdateSystemSetting(String organizationId, SystemSettingsRequest request,
            String userId) {
        SystemSetting setting = systemSettingRepository.findByOrganizationIdAndKey(organizationId, request.getKey())
                .orElse(SystemSetting.builder()
                        .organizationId(organizationId)
                        .key(request.getKey())
                        .build());

        setting.setValue(request.getValue());
        setting.setDescription(request.getDescription());
        setting.setSettingType(request.getSettingType());
        setting.setIsDefault(request.getIsDefault() != null ? request.getIsDefault() : false);

        SystemSetting savedSetting = systemSettingRepository.save(setting);
        return mapToResponse(savedSetting);
    }

    /**
     * Update system setting by key
     */
    public SystemSettingsResponse updateSystemSettingByKey(String organizationId, String key,
            SystemSettingsRequest request, String userId) {
        SystemSetting setting = systemSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "System setting not found for organization: " + organizationId + ", key: " + key));

        if (request.getValue() != null) {
            setting.setValue(request.getValue());
        }
        if (request.getDescription() != null) {
            setting.setDescription(request.getDescription());
        }
        if (request.getSettingType() != null) {
            setting.setSettingType(request.getSettingType());
        }
        if (request.getIsDefault() != null) {
            setting.setIsDefault(request.getIsDefault());
        }

        SystemSetting updatedSetting = systemSettingRepository.save(setting);
        return mapToResponse(updatedSetting);
    }

    /**
     * Delete system setting by key
     */
    public void deleteSystemSettingByKey(String organizationId, String key) {
        SystemSetting setting = systemSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "System setting not found for organization: " + organizationId + ", key: " + key));

        systemSettingRepository.delete(setting);
    }

    /**
     * Get system settings by setting type
     */
    public List<SystemSettingsResponse> getSystemSettingsByCategory(String organizationId, String settingType) {
        return systemSettingRepository.findByOrganizationIdAndSettingType(organizationId, settingType)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get default system settings for an organization
     */
    public List<SystemSettingsResponse> getDefaultSystemSettings(String organizationId) {
        return systemSettingRepository.findByOrganizationIdAndIsDefaultTrue(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Initialize default system settings for a new organization
     */
    public void initializeDefaultSettings(String organizationId) {
        List<SystemSetting> defaultSettings = createDefaultSystemSettings(organizationId);
        systemSettingRepository.saveAll(defaultSettings);
        log.info("Initialized default system settings for organization: {}", organizationId);
    }

    /**
     * Create default system settings
     */
    private List<SystemSetting> createDefaultSystemSettings(String organizationId) {
        return List.of(
                // Security settings
                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("security.session_timeout")
                        .value("30")
                        .description("Session timeout in minutes")
                        .settingType("security")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("security.password_policy")
                        .value("strong")
                        .description("Password policy: basic, strong, or strict")
                        .settingType("security")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("security.two_factor_auth")
                        .value("false")
                        .description("Enable two-factor authentication")
                        .settingType("security")
                        .isDefault(true)
                        .build(),

                // Performance settings
                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("performance.cache_enabled")
                        .value("true")
                        .description("Enable system caching")
                        .settingType("performance")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("performance.max_connections")
                        .value("100")
                        .description("Maximum database connections")
                        .settingType("performance")
                        .isDefault(true)
                        .build(),

                // Backup settings
                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("backup.auto_backup")
                        .value("true")
                        .description("Enable automatic backups")
                        .settingType("backup")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("backup.retention_days")
                        .value("30")
                        .description("Backup retention period in days")
                        .settingType("backup")
                        .isDefault(true)
                        .build(),

                // General settings
                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("general.timezone")
                        .value("UTC")
                        .description("System timezone")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("general.language")
                        .value("en")
                        .description("System language")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                SystemSetting.builder()
                        .organizationId(organizationId)
                        .key("general.currency")
                        .value("USD")
                        .description("Default currency")
                        .settingType("general")
                        .isDefault(true)
                        .build());
    }

    /**
     * Map SystemSetting entity to SystemSettingsResponse DTO
     */
    private SystemSettingsResponse mapToResponse(SystemSetting setting) {
        return SystemSettingsResponse.builder()
                .id(setting.getId())
                .organizationId(setting.getOrganizationId())
                .key(setting.getKey())
                .value(setting.getValue())
                .description(setting.getDescription())
                .settingType(setting.getSettingType())
                .isDefault(setting.getIsDefault())
                .createdAt(setting.getCreatedAt())
                .updatedAt(setting.getUpdatedAt())
                .build();
    }
}