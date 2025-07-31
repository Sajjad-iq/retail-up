package com.sajjadkademm.retail.settings.inventory.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import com.sajjadkademm.retail.settings.inventory.repository.InventorySettingRepository;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Optional;

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
                                .orElseGet(() -> createDefaultInventorySettings(organizationId));

                return mapToResponse(setting);
        }

        /**
         * Create or update inventory settings
         */
        public InventorySettingsResponse createOrUpdateInventorySettings(String organizationId,
                        InventorySettingsRequest request, String userId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseGet(() -> createDefaultInventorySettings(organizationId));

                updateInventorySetting(setting, request);
                InventorySetting savedSetting = inventorySettingRepository.save(setting);
                return mapToResponse(savedSetting);
        }

        /**
         * Update inventory settings
         */
        public InventorySettingsResponse updateInventorySettings(String organizationId,
                        InventorySettingsRequest request, String userId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseThrow(() -> new NotFoundException(
                                                "Inventory settings not found for organization: " + organizationId));

                updateInventorySetting(setting, request);
                InventorySetting updatedSetting = inventorySettingRepository.save(setting);
                return mapToResponse(updatedSetting);
        }

        /**
         * Initialize default inventory settings for a new organization
         */
        public void initializeDefaultSettings(String organizationId) {
                if (!inventorySettingRepository.existsByOrganizationId(organizationId)) {
                        InventorySetting defaultSettings = createDefaultInventorySettings(organizationId);
                        inventorySettingRepository.save(defaultSettings);
                        log.info("Initialized default inventory settings for organization: {}", organizationId);
                }
        }

        /**
         * Reset inventory settings to defaults
         */
        public InventorySettingsResponse resetToDefaults(String organizationId) {
                InventorySetting setting = inventorySettingRepository.findByOrganizationId(organizationId)
                                .orElseGet(() -> createDefaultInventorySettings(organizationId));

                InventorySetting defaultSettings = createDefaultInventorySettings(organizationId);
                defaultSettings.setId(setting.getId());
                defaultSettings.setCreatedAt(setting.getCreatedAt());

                InventorySetting savedSetting = inventorySettingRepository.save(defaultSettings);
                return mapToResponse(savedSetting);
        }

        /**
         * Create default inventory settings
         */
        private InventorySetting createDefaultInventorySettings(String organizationId) {
                return InventorySetting.builder()
                                .organizationId(organizationId)
                                // Stock Management Settings
                                .negativeStockAllowed(false)
                                .autoStockAdjustment(true)
                                .barcodeRequired(true)
                                .skuRequired(true)
                                .allowZeroPricing(false)
                                .requireCostPrice(true)
                                .autoCalculateMarkup(false)
                                .defaultMarkupPercentage(30.0)
                                // Alert Settings
                                .lowStockAlertsEnabled(true)
                                .lowStockThresholdPercentage(10)
                                .outOfStockAlertsEnabled(true)
                                .expiryAlertsEnabled(true)
                                .expiryAlertDays(30)
                                .overstockAlertsEnabled(false)
                                .overstockThresholdPercentage(200)
                                .alertEmailEnabled(true)
                                .alertSmsEnabled(false)
                                // Category Settings
                                .autoCategorizationEnabled(false)
                                .maxCategoriesPerItem(3)
                                .requireCategoryAssignment(true)
                                .allowSubcategories(true)
                                .maxSubcategoryLevels(3)
                                // Reorder Settings
                                .autoReorderEnabled(false)
                                .reorderPointCalculation("lead_time_demand")
                                .safetyStockPercentage(20)
                                .leadTimeDays(7)
                                .reorderQuantityCalculation("eoq")
                                .economicOrderQuantityEnabled(true)
                                .minOrderQuantity(1)
                                .maxOrderQuantity(10000)
                                // Tracking Settings
                                .inventoryTrackingMethod("fifo")
                                .batchTrackingEnabled(true)
                                .serialNumberTrackingEnabled(false)
                                .lotNumberRequired(false)
                                .expiryDateTrackingEnabled(true)
                                .manufacturingDateTrackingEnabled(false)
                                // Supplier Settings
                                .supplierManagementEnabled(true)
                                .requireSupplierAssignment(false)
                                .multipleSuppliersPerItem(true)
                                .preferredSupplierEnabled(true)
                                // Location Settings
                                .multiLocationEnabled(false)
                                .warehouseManagementEnabled(false)
                                .binLocationTrackingEnabled(false)
                                .requireLocationAssignment(false)
                                .build();
        }

        /**
         * Update inventory setting with request data
         */
        private void updateInventorySetting(InventorySetting setting, InventorySettingsRequest request) {
                // Stock Management Settings
                if (request.getNegativeStockAllowed() != null) {
                        setting.setNegativeStockAllowed(request.getNegativeStockAllowed());
                }
                if (request.getAutoStockAdjustment() != null) {
                        setting.setAutoStockAdjustment(request.getAutoStockAdjustment());
                }
                if (request.getBarcodeRequired() != null) {
                        setting.setBarcodeRequired(request.getBarcodeRequired());
                }
                if (request.getSkuRequired() != null) {
                        setting.setSkuRequired(request.getSkuRequired());
                }
                if (request.getAllowZeroPricing() != null) {
                        setting.setAllowZeroPricing(request.getAllowZeroPricing());
                }
                if (request.getRequireCostPrice() != null) {
                        setting.setRequireCostPrice(request.getRequireCostPrice());
                }
                if (request.getAutoCalculateMarkup() != null) {
                        setting.setAutoCalculateMarkup(request.getAutoCalculateMarkup());
                }
                if (request.getDefaultMarkupPercentage() != null) {
                        setting.setDefaultMarkupPercentage(request.getDefaultMarkupPercentage());
                }

                // Alert Settings
                if (request.getLowStockAlertsEnabled() != null) {
                        setting.setLowStockAlertsEnabled(request.getLowStockAlertsEnabled());
                }
                if (request.getLowStockThresholdPercentage() != null) {
                        setting.setLowStockThresholdPercentage(request.getLowStockThresholdPercentage());
                }
                if (request.getOutOfStockAlertsEnabled() != null) {
                        setting.setOutOfStockAlertsEnabled(request.getOutOfStockAlertsEnabled());
                }
                if (request.getExpiryAlertsEnabled() != null) {
                        setting.setExpiryAlertsEnabled(request.getExpiryAlertsEnabled());
                }
                if (request.getExpiryAlertDays() != null) {
                        setting.setExpiryAlertDays(request.getExpiryAlertDays());
                }
                if (request.getOverstockAlertsEnabled() != null) {
                        setting.setOverstockAlertsEnabled(request.getOverstockAlertsEnabled());
                }
                if (request.getOverstockThresholdPercentage() != null) {
                        setting.setOverstockThresholdPercentage(request.getOverstockThresholdPercentage());
                }
                if (request.getAlertEmailEnabled() != null) {
                        setting.setAlertEmailEnabled(request.getAlertEmailEnabled());
                }
                if (request.getAlertSmsEnabled() != null) {
                        setting.setAlertSmsEnabled(request.getAlertSmsEnabled());
                }

                // Category Settings
                if (request.getAutoCategorizationEnabled() != null) {
                        setting.setAutoCategorizationEnabled(request.getAutoCategorizationEnabled());
                }
                if (request.getMaxCategoriesPerItem() != null) {
                        setting.setMaxCategoriesPerItem(request.getMaxCategoriesPerItem());
                }
                if (request.getRequireCategoryAssignment() != null) {
                        setting.setRequireCategoryAssignment(request.getRequireCategoryAssignment());
                }
                if (request.getAllowSubcategories() != null) {
                        setting.setAllowSubcategories(request.getAllowSubcategories());
                }
                if (request.getMaxSubcategoryLevels() != null) {
                        setting.setMaxSubcategoryLevels(request.getMaxSubcategoryLevels());
                }

                // Reorder Settings
                if (request.getAutoReorderEnabled() != null) {
                        setting.setAutoReorderEnabled(request.getAutoReorderEnabled());
                }
                if (request.getReorderPointCalculation() != null) {
                        setting.setReorderPointCalculation(request.getReorderPointCalculation());
                }
                if (request.getSafetyStockPercentage() != null) {
                        setting.setSafetyStockPercentage(request.getSafetyStockPercentage());
                }
                if (request.getLeadTimeDays() != null) {
                        setting.setLeadTimeDays(request.getLeadTimeDays());
                }
                if (request.getReorderQuantityCalculation() != null) {
                        setting.setReorderQuantityCalculation(request.getReorderQuantityCalculation());
                }
                if (request.getEconomicOrderQuantityEnabled() != null) {
                        setting.setEconomicOrderQuantityEnabled(request.getEconomicOrderQuantityEnabled());
                }
                if (request.getMinOrderQuantity() != null) {
                        setting.setMinOrderQuantity(request.getMinOrderQuantity());
                }
                if (request.getMaxOrderQuantity() != null) {
                        setting.setMaxOrderQuantity(request.getMaxOrderQuantity());
                }

                // Tracking Settings
                if (request.getInventoryTrackingMethod() != null) {
                        setting.setInventoryTrackingMethod(request.getInventoryTrackingMethod());
                }
                if (request.getBatchTrackingEnabled() != null) {
                        setting.setBatchTrackingEnabled(request.getBatchTrackingEnabled());
                }
                if (request.getSerialNumberTrackingEnabled() != null) {
                        setting.setSerialNumberTrackingEnabled(request.getSerialNumberTrackingEnabled());
                }
                if (request.getLotNumberRequired() != null) {
                        setting.setLotNumberRequired(request.getLotNumberRequired());
                }
                if (request.getExpiryDateTrackingEnabled() != null) {
                        setting.setExpiryDateTrackingEnabled(request.getExpiryDateTrackingEnabled());
                }
                if (request.getManufacturingDateTrackingEnabled() != null) {
                        setting.setManufacturingDateTrackingEnabled(request.getManufacturingDateTrackingEnabled());
                }

                // Supplier Settings
                if (request.getSupplierManagementEnabled() != null) {
                        setting.setSupplierManagementEnabled(request.getSupplierManagementEnabled());
                }
                if (request.getRequireSupplierAssignment() != null) {
                        setting.setRequireSupplierAssignment(request.getRequireSupplierAssignment());
                }
                if (request.getMultipleSuppliersPerItem() != null) {
                        setting.setMultipleSuppliersPerItem(request.getMultipleSuppliersPerItem());
                }
                if (request.getPreferredSupplierEnabled() != null) {
                        setting.setPreferredSupplierEnabled(request.getPreferredSupplierEnabled());
                }

                // Location Settings
                if (request.getMultiLocationEnabled() != null) {
                        setting.setMultiLocationEnabled(request.getMultiLocationEnabled());
                }
                if (request.getWarehouseManagementEnabled() != null) {
                        setting.setWarehouseManagementEnabled(request.getWarehouseManagementEnabled());
                }
                if (request.getBinLocationTrackingEnabled() != null) {
                        setting.setBinLocationTrackingEnabled(request.getBinLocationTrackingEnabled());
                }
                if (request.getRequireLocationAssignment() != null) {
                        setting.setRequireLocationAssignment(request.getRequireLocationAssignment());
                }
        }

        /**
         * Map InventorySetting entity to InventorySettingsResponse DTO
         */
        private InventorySettingsResponse mapToResponse(InventorySetting setting) {
                return InventorySettingsResponse.builder()
                                .id(setting.getId())
                                .organizationId(setting.getOrganizationId())
                                // Stock Management Settings
                                .negativeStockAllowed(setting.getNegativeStockAllowed())
                                .autoStockAdjustment(setting.getAutoStockAdjustment())
                                .barcodeRequired(setting.getBarcodeRequired())
                                .skuRequired(setting.getSkuRequired())
                                .allowZeroPricing(setting.getAllowZeroPricing())
                                .requireCostPrice(setting.getRequireCostPrice())
                                .autoCalculateMarkup(setting.getAutoCalculateMarkup())
                                .defaultMarkupPercentage(setting.getDefaultMarkupPercentage())
                                // Alert Settings
                                .lowStockAlertsEnabled(setting.getLowStockAlertsEnabled())
                                .lowStockThresholdPercentage(setting.getLowStockThresholdPercentage())
                                .outOfStockAlertsEnabled(setting.getOutOfStockAlertsEnabled())
                                .expiryAlertsEnabled(setting.getExpiryAlertsEnabled())
                                .expiryAlertDays(setting.getExpiryAlertDays())
                                .overstockAlertsEnabled(setting.getOverstockAlertsEnabled())
                                .overstockThresholdPercentage(setting.getOverstockThresholdPercentage())
                                .alertEmailEnabled(setting.getAlertEmailEnabled())
                                .alertSmsEnabled(setting.getAlertSmsEnabled())
                                // Category Settings
                                .autoCategorizationEnabled(setting.getAutoCategorizationEnabled())
                                .maxCategoriesPerItem(setting.getMaxCategoriesPerItem())
                                .requireCategoryAssignment(setting.getRequireCategoryAssignment())
                                .allowSubcategories(setting.getAllowSubcategories())
                                .maxSubcategoryLevels(setting.getMaxSubcategoryLevels())
                                // Reorder Settings
                                .autoReorderEnabled(setting.getAutoReorderEnabled())
                                .reorderPointCalculation(setting.getReorderPointCalculation())
                                .safetyStockPercentage(setting.getSafetyStockPercentage())
                                .leadTimeDays(setting.getLeadTimeDays())
                                .reorderQuantityCalculation(setting.getReorderQuantityCalculation())
                                .economicOrderQuantityEnabled(setting.getEconomicOrderQuantityEnabled())
                                .minOrderQuantity(setting.getMinOrderQuantity())
                                .maxOrderQuantity(setting.getMaxOrderQuantity())
                                // Tracking Settings
                                .inventoryTrackingMethod(setting.getInventoryTrackingMethod())
                                .batchTrackingEnabled(setting.getBatchTrackingEnabled())
                                .serialNumberTrackingEnabled(setting.getSerialNumberTrackingEnabled())
                                .lotNumberRequired(setting.getLotNumberRequired())
                                .expiryDateTrackingEnabled(setting.getExpiryDateTrackingEnabled())
                                .manufacturingDateTrackingEnabled(setting.getManufacturingDateTrackingEnabled())
                                // Supplier Settings
                                .supplierManagementEnabled(setting.getSupplierManagementEnabled())
                                .requireSupplierAssignment(setting.getRequireSupplierAssignment())
                                .multipleSuppliersPerItem(setting.getMultipleSuppliersPerItem())
                                .preferredSupplierEnabled(setting.getPreferredSupplierEnabled())
                                // Location Settings
                                .multiLocationEnabled(setting.getMultiLocationEnabled())
                                .warehouseManagementEnabled(setting.getWarehouseManagementEnabled())
                                .binLocationTrackingEnabled(setting.getBinLocationTrackingEnabled())
                                .requireLocationAssignment(setting.getRequireLocationAssignment())
                                .createdAt(setting.getCreatedAt())
                                .updatedAt(setting.getUpdatedAt())
                                .build();
        }
}