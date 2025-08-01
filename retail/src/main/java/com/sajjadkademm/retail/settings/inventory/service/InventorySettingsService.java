package com.sajjadkademm.retail.settings.inventory.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import com.sajjadkademm.retail.settings.inventory.repository.InventorySettingRepository;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InventorySettingsService {

        private final InventorySettingRepository inventorySettingRepository;

        /**
         * Get inventory settings for an organization
         */
        public InventorySettingsResponse getInventorySettings(String organizationId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                return mapToResponse(setting);
        }

        /**
         * Update inventory settings
         */
        public InventorySettingsResponse updateInventorySettings(String organizationId,
                        InventorySettingsRequest request, String userId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                updateInventorySetting(setting, request, userId);
                InventorySetting updatedSetting = inventorySettingRepository.save(setting);
                return mapToResponse(updatedSetting);
        }

        /**
         * Reset inventory settings to defaults
         */
        public InventorySettingsResponse resetToDefaults(String organizationId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                InventorySetting defaultSettings = createDefaultInventorySettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                InventorySetting savedSetting = inventorySettingRepository.save(defaultSettings);
                return mapToResponse(savedSetting);
        }

        /**
         * Create default inventory settings (used when creating organization)
         */
        public InventorySetting createDefaultInventorySettings(String organizationId) {
                return InventorySetting.builder()
                                .organizationId(organizationId)
                                // Stock Management Settings
                                .negativeStockAllowed(false)
                                .barcodeRequired(true)
                                .skuRequired(true)
                                .requireCostPrice(true)
                                // Alert Settings
                                .lowStockAlertsEnabled(true)
                                .lowStockThreshold(10)
                                .outOfStockAlertsEnabled(true)
                                .expiryAlertsEnabled(true)
                                .expiryAlertDays(30)
                                // Tracking Settings
                                .batchTrackingEnabled(true)
                                .expiryDateTrackingEnabled(true)
                                .build();
        }

        /**
         * Update inventory setting with request data
         */
        private void updateInventorySetting(InventorySetting setting, InventorySettingsRequest request, String userId) {
                // Stock Management Settings
                setting.setNegativeStockAllowed(request.getNegativeStockAllowed());
                setting.setBarcodeRequired(request.getBarcodeRequired());
                setting.setSkuRequired(request.getSkuRequired());
                setting.setRequireCostPrice(request.getRequireCostPrice());

                // Alert Settings
                setting.setLowStockAlertsEnabled(request.getLowStockAlertsEnabled());
                setting.setLowStockThreshold(request.getLowStockThreshold());
                setting.setOutOfStockAlertsEnabled(request.getOutOfStockAlertsEnabled());
                setting.setExpiryAlertsEnabled(request.getExpiryAlertsEnabled());
                setting.setExpiryAlertDays(request.getExpiryAlertDays());

                // Tracking Settings
                setting.setBatchTrackingEnabled(request.getBatchTrackingEnabled());
                setting.setExpiryDateTrackingEnabled(request.getExpiryDateTrackingEnabled());

                // Audit Fields
                setting.setUpdatedBy(request.getUpdatedBy() != null ? request.getUpdatedBy() : userId);
        }

        /**
         * Map entity to response DTO
         */
        private InventorySettingsResponse mapToResponse(InventorySetting setting) {
                return InventorySettingsResponse.builder()
                                .id(setting.getId())
                                .organizationId(setting.getOrganizationId())
                                // Stock Management Settings
                                .negativeStockAllowed(setting.getNegativeStockAllowed())
                                .barcodeRequired(setting.getBarcodeRequired())
                                .skuRequired(setting.getSkuRequired())
                                .requireCostPrice(setting.getRequireCostPrice())
                                // Alert Settings
                                .lowStockAlertsEnabled(setting.getLowStockAlertsEnabled())
                                .lowStockThreshold(setting.getLowStockThreshold())
                                .outOfStockAlertsEnabled(setting.getOutOfStockAlertsEnabled())
                                .expiryAlertsEnabled(setting.getExpiryAlertsEnabled())
                                .expiryAlertDays(setting.getExpiryAlertDays())
                                // Tracking Settings
                                .batchTrackingEnabled(setting.getBatchTrackingEnabled())
                                .expiryDateTrackingEnabled(setting.getExpiryDateTrackingEnabled())
                                // Audit Fields
                                .updatedBy(setting.getUpdatedBy())
                                .createdAt(setting.getCreatedAt())
                                .updatedAt(setting.getUpdatedAt())
                                .build();
        }
}