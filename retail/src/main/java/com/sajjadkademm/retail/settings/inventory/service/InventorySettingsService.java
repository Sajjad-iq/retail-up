package com.sajjadkademm.retail.settings.inventory.service;

import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import com.sajjadkademm.retail.settings.inventory.repository.InventorySettingRepository;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class InventorySettingsService {

        private final InventorySettingRepository inventorySettingRepository;

        /**
         * Get inventory settings for an organization
         */
        public InventorySetting getInventorySettings(String organizationId) {
                return inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));
        }

        /**
         * Update inventory settings
         */
        public InventorySetting updateInventorySettings(String organizationId,
                        InventorySettingsRequest request) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                updateInventorySetting(setting, request);
                return inventorySettingRepository.save(setting);
        }

        /**
         * Reset inventory settings to defaults
         */
        public InventorySetting resetToDefaults(String organizationId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                InventorySetting defaultSettings = createDefaultInventorySettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                return inventorySettingRepository.save(defaultSettings);
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
         * Create and save default inventory settings for a new organization
         */
        public InventorySetting createAndSaveDefaultInventorySettings(String organizationId, String createdBy) {
                try {
                        InventorySetting defaultSettings = createDefaultInventorySettings(organizationId);
                        return inventorySettingRepository.save(defaultSettings);
                } catch (Exception e) {
                        throw new BadRequestException("Failed to create default inventory settings: " + e.getMessage(),
                                        e);
                }
        }

        /**
         * Update inventory setting with request data
         */
        private void updateInventorySetting(InventorySetting setting, InventorySettingsRequest request) {
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
        }
}