package com.sajjadkademm.retail.settings.pos.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import com.sajjadkademm.retail.settings.pos.repository.POSSettingRepository;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class POSSettingsService {

    private final POSSettingRepository posSettingRepository;

    /**
     * Get all POS settings for an organization
     */
    public List<POSSettingsResponse> getPOSSettings(String organizationId) {
        return posSettingRepository.findByOrganizationId(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get POS setting by key
     */
    public POSSettingsResponse getPOSSettingByKey(String organizationId, String key) {
        POSSetting setting = posSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "POS setting not found for organization: " + organizationId + ", key: " + key));

        return mapToResponse(setting);
    }

    /**
     * Create or update POS setting
     */
    public POSSettingsResponse createOrUpdatePOSSetting(String organizationId, POSSettingsRequest request,
            String userId) {
        POSSetting setting = posSettingRepository.findByOrganizationIdAndKey(organizationId, request.getKey())
                .orElse(POSSetting.builder()
                        .organizationId(organizationId)
                        .key(request.getKey())
                        .build());

        setting.setValue(request.getValue());
        setting.setDescription(request.getDescription());
        setting.setSettingType(request.getSettingType());
        setting.setIsDefault(request.getIsDefault() != null ? request.getIsDefault() : false);

        POSSetting savedSetting = posSettingRepository.save(setting);
        return mapToResponse(savedSetting);
    }

    /**
     * Update POS setting by key
     */
    public POSSettingsResponse updatePOSSettingByKey(String organizationId, String key, POSSettingsRequest request,
            String userId) {
        POSSetting setting = posSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "POS setting not found for organization: " + organizationId + ", key: " + key));

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

        POSSetting updatedSetting = posSettingRepository.save(setting);
        return mapToResponse(updatedSetting);
    }

    /**
     * Delete POS setting by key
     */
    public void deletePOSSettingByKey(String organizationId, String key) {
        POSSetting setting = posSettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "POS setting not found for organization: " + organizationId + ", key: " + key));

        posSettingRepository.delete(setting);
    }

    /**
     * Get POS settings by setting type
     */
    public List<POSSettingsResponse> getPOSSettingsByCategory(String organizationId, String settingType) {
        return posSettingRepository.findByOrganizationIdAndSettingType(organizationId, settingType)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get default POS settings for an organization
     */
    public List<POSSettingsResponse> getDefaultPOSSettings(String organizationId) {
        return posSettingRepository.findByOrganizationIdAndIsDefaultTrue(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get payment methods configuration
     */
    public List<POSSettingsResponse> getPaymentMethods(String organizationId) {
        return posSettingRepository.findByOrganizationIdAndSettingType(organizationId, "payment")
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get tax configuration
     */
    public List<POSSettingsResponse> getTaxConfig(String organizationId) {
        return posSettingRepository.findByOrganizationIdAndSettingType(organizationId, "tax")
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Initialize default POS settings for a new organization
     */
    public void initializeDefaultSettings(String organizationId) {
        List<POSSetting> defaultSettings = createDefaultPOSSettings(organizationId);
        posSettingRepository.saveAll(defaultSettings);
        log.info("Initialized default POS settings for organization: {}", organizationId);
    }

    /**
     * Create default POS settings
     */
    private List<POSSetting> createDefaultPOSSettings(String organizationId) {
        return List.of(
                // Payment settings
                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("payment.cash_enabled")
                        .value("true")
                        .description("Enable cash payments")
                        .settingType("payment")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("payment.card_enabled")
                        .value("true")
                        .description("Enable card payments")
                        .settingType("payment")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("payment.mobile_enabled")
                        .value("false")
                        .description("Enable mobile payments")
                        .settingType("payment")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("payment.change_calculation")
                        .value("automatic")
                        .description("Change calculation method: automatic or manual")
                        .settingType("payment")
                        .isDefault(true)
                        .build(),

                // Receipt settings
                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("receipt.auto_print")
                        .value("true")
                        .description("Automatically print receipts")
                        .settingType("receipt")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("receipt.show_tax")
                        .value("true")
                        .description("Show tax breakdown on receipts")
                        .settingType("receipt")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("receipt.footer_text")
                        .value("Thank you for your purchase!")
                        .description("Receipt footer text")
                        .settingType("receipt")
                        .isDefault(true)
                        .build(),

                // Tax settings
                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("tax.enabled")
                        .value("true")
                        .description("Enable tax calculation")
                        .settingType("tax")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("tax.rate")
                        .value("8.5")
                        .description("Default tax rate percentage")
                        .settingType("tax")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("tax.inclusive")
                        .value("false")
                        .description("Tax inclusive pricing")
                        .settingType("tax")
                        .isDefault(true)
                        .build(),

                // General POS settings
                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("general.hold_transactions")
                        .value("true")
                        .description("Allow holding transactions")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("general.returns_enabled")
                        .value("true")
                        .description("Enable returns and refunds")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                POSSetting.builder()
                        .organizationId(organizationId)
                        .key("general.discounts_enabled")
                        .value("true")
                        .description("Enable discounts and promotions")
                        .settingType("general")
                        .isDefault(true)
                        .build());
    }

    /**
     * Map POSSetting entity to POSSettingsResponse DTO
     */
    private POSSettingsResponse mapToResponse(POSSetting setting) {
        return POSSettingsResponse.builder()
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