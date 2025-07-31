package com.sajjadkademm.retail.settings.inventory.service;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import com.sajjadkademm.retail.settings.inventory.repository.InventorySettingRepository;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class InventorySettingsService {

    private final InventorySettingRepository inventorySettingRepository;

    /**
     * Get all inventory settings for an organization
     */
    public List<InventorySettingsResponse> getInventorySettings(String organizationId) {
        return inventorySettingRepository.findByOrganizationId(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get inventory setting by key
     */
    public InventorySettingsResponse getInventorySettingByKey(String organizationId, String key) {
        InventorySetting setting = inventorySettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "Inventory setting not found for organization: " + organizationId + ", key: " + key));

        return mapToResponse(setting);
    }

    /**
     * Create or update inventory setting
     */
    public InventorySettingsResponse createOrUpdateInventorySetting(String organizationId,
            InventorySettingsRequest request, String userId) {
        InventorySetting setting = inventorySettingRepository
                .findByOrganizationIdAndKey(organizationId, request.getKey())
                .orElse(InventorySetting.builder()
                        .organizationId(organizationId)
                        .key(request.getKey())
                        .build());

        setting.setValue(request.getValue());
        setting.setDescription(request.getDescription());
        setting.setSettingType(request.getSettingType());
        setting.setIsDefault(request.getIsDefault() != null ? request.getIsDefault() : false);

        InventorySetting savedSetting = inventorySettingRepository.save(setting);
        return mapToResponse(savedSetting);
    }

    /**
     * Update inventory setting by key
     */
    public InventorySettingsResponse updateInventorySettingByKey(String organizationId, String key,
            InventorySettingsRequest request, String userId) {
        InventorySetting setting = inventorySettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "Inventory setting not found for organization: " + organizationId + ", key: " + key));

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

        InventorySetting updatedSetting = inventorySettingRepository.save(setting);
        return mapToResponse(updatedSetting);
    }

    /**
     * Delete inventory setting by key
     */
    public void deleteInventorySettingByKey(String organizationId, String key) {
        InventorySetting setting = inventorySettingRepository.findByOrganizationIdAndKey(organizationId, key)
                .orElseThrow(() -> new NotFoundException(
                        "Inventory setting not found for organization: " + organizationId + ", key: " + key));

        inventorySettingRepository.delete(setting);
    }

    /**
     * Get inventory settings by setting type
     */
    public List<InventorySettingsResponse> getInventorySettingsByCategory(String organizationId, String settingType) {
        return inventorySettingRepository.findByOrganizationIdAndSettingType(organizationId, settingType)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get default inventory settings for an organization
     */
    public List<InventorySettingsResponse> getDefaultInventorySettings(String organizationId) {
        return inventorySettingRepository.findByOrganizationIdAndIsDefaultTrue(organizationId)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get stock alert configuration
     */
    public List<InventorySettingsResponse> getStockAlerts(String organizationId) {
        return inventorySettingRepository.findByOrganizationIdAndSettingType(organizationId, "alerts")
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get inventory categories configuration
     */
    public List<InventorySettingsResponse> getInventoryCategories(String organizationId) {
        return inventorySettingRepository.findByOrganizationIdAndSettingType(organizationId, "categories")
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Get reorder point configuration
     */
    public List<InventorySettingsResponse> getReorderPoints(String organizationId) {
        return inventorySettingRepository.findByOrganizationIdAndSettingType(organizationId, "reorder")
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Initialize default inventory settings for a new organization
     */
    public void initializeDefaultSettings(String organizationId) {
        List<InventorySetting> defaultSettings = createDefaultInventorySettings(organizationId);
        inventorySettingRepository.saveAll(defaultSettings);
        log.info("Initialized default inventory settings for organization: {}", organizationId);
    }

    /**
     * Create default inventory settings
     */
    private List<InventorySetting> createDefaultInventorySettings(String organizationId) {
        return List.of(
                // Stock management settings
                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("stock.negative_stock_allowed")
                        .value("false")
                        .description("Allow negative stock levels")
                        .settingType("stock")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("stock.auto_adjustment")
                        .value("true")
                        .description("Enable automatic stock adjustments")
                        .settingType("stock")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("stock.barcode_required")
                        .value("true")
                        .description("Require barcode for all items")
                        .settingType("stock")
                        .isDefault(true)
                        .build(),

                // Alert settings
                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("alerts.low_stock_enabled")
                        .value("true")
                        .description("Enable low stock alerts")
                        .settingType("alerts")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("alerts.low_stock_threshold")
                        .value("10")
                        .description("Low stock threshold percentage")
                        .settingType("alerts")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("alerts.out_of_stock_enabled")
                        .value("true")
                        .description("Enable out of stock alerts")
                        .settingType("alerts")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("alerts.expiry_alerts_enabled")
                        .value("true")
                        .description("Enable expiry date alerts")
                        .settingType("alerts")
                        .isDefault(true)
                        .build(),

                // Category settings
                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("categories.auto_categorization")
                        .value("false")
                        .description("Enable automatic item categorization")
                        .settingType("categories")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("categories.max_categories_per_item")
                        .value("3")
                        .description("Maximum categories per item")
                        .settingType("categories")
                        .isDefault(true)
                        .build(),

                // Reorder settings
                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("reorder.auto_reorder_enabled")
                        .value("false")
                        .description("Enable automatic reordering")
                        .settingType("reorder")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("reorder.reorder_point_calculation")
                        .value("lead_time_demand")
                        .description("Reorder point calculation method")
                        .settingType("reorder")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("reorder.safety_stock_percentage")
                        .value("20")
                        .description("Safety stock as percentage of demand")
                        .settingType("reorder")
                        .isDefault(true)
                        .build(),

                // General inventory settings
                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("general.inventory_tracking")
                        .value("fifo")
                        .description("Inventory tracking method: fifo, lifo, or average")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("general.batch_tracking")
                        .value("true")
                        .description("Enable batch/lot tracking")
                        .settingType("general")
                        .isDefault(true)
                        .build(),

                InventorySetting.builder()
                        .organizationId(organizationId)
                        .key("general.serial_number_tracking")
                        .value("false")
                        .description("Enable serial number tracking")
                        .settingType("general")
                        .isDefault(true)
                        .build());
    }

    /**
     * Map InventorySetting entity to InventorySettingsResponse DTO
     */
    private InventorySettingsResponse mapToResponse(InventorySetting setting) {
        return InventorySettingsResponse.builder()
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