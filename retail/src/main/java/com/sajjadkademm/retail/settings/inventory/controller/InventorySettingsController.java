package com.sajjadkademm.retail.settings.inventory.controller;

import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/settings/inventory")
@RequiredArgsConstructor
public class InventorySettingsController {

    private final InventorySettingsService inventorySettingsService;

    /**
     * Get all inventory settings for an organization
     */
    @GetMapping("/organization/{organizationId}")
    public ResponseEntity<List<InventorySettingsResponse>> getInventorySettings(@PathVariable String organizationId) {
        List<InventorySettingsResponse> response = inventorySettingsService.getInventorySettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get specific inventory setting by key
     */
    @GetMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<InventorySettingsResponse> getInventorySettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        InventorySettingsResponse response = inventorySettingsService.getInventorySettingByKey(organizationId, key);
        return ResponseEntity.ok(response);
    }

    /**
     * Create or update inventory setting
     */
    @PostMapping("/organization/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> createOrUpdateInventorySetting(
            @PathVariable String organizationId,
            @Valid @RequestBody InventorySettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        InventorySettingsResponse response = inventorySettingsService.createOrUpdateInventorySetting(organizationId,
                request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update inventory setting by key
     */
    @PutMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<InventorySettingsResponse> updateInventorySettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key,
            @Valid @RequestBody InventorySettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        InventorySettingsResponse response = inventorySettingsService.updateInventorySettingByKey(organizationId, key,
                request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Delete inventory setting by key
     */
    @DeleteMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<Void> deleteInventorySettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        inventorySettingsService.deleteInventorySettingByKey(organizationId, key);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get inventory settings by category (e.g., "stock", "alerts", "categories")
     */
    @GetMapping("/organization/{organizationId}/category/{category}")
    public ResponseEntity<List<InventorySettingsResponse>> getInventorySettingsByCategory(
            @PathVariable String organizationId,
            @PathVariable String category) {
        List<InventorySettingsResponse> response = inventorySettingsService
                .getInventorySettingsByCategory(organizationId, category);
        return ResponseEntity.ok(response);
    }

    /**
     * Get stock alert configuration
     */
    @GetMapping("/organization/{organizationId}/stock-alerts")
    public ResponseEntity<List<InventorySettingsResponse>> getStockAlerts(@PathVariable String organizationId) {
        List<InventorySettingsResponse> response = inventorySettingsService.getStockAlerts(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get inventory categories configuration
     */
    @GetMapping("/organization/{organizationId}/categories")
    public ResponseEntity<List<InventorySettingsResponse>> getInventoryCategories(@PathVariable String organizationId) {
        List<InventorySettingsResponse> response = inventorySettingsService.getInventoryCategories(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get reorder point configuration
     */
    @GetMapping("/organization/{organizationId}/reorder-points")
    public ResponseEntity<List<InventorySettingsResponse>> getReorderPoints(@PathVariable String organizationId) {
        List<InventorySettingsResponse> response = inventorySettingsService.getReorderPoints(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get default inventory settings for an organization
     */
    @GetMapping("/organization/{organizationId}/defaults")
    public ResponseEntity<List<InventorySettingsResponse>> getDefaultInventorySettings(
            @PathVariable String organizationId) {
        List<InventorySettingsResponse> response = inventorySettingsService.getDefaultInventorySettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Initialize default inventory settings for a new organization
     */
    @PostMapping("/organization/{organizationId}/initialize")
    public ResponseEntity<Void> initializeDefaultInventorySettings(@PathVariable String organizationId) {
        inventorySettingsService.initializeDefaultSettings(organizationId);
        return ResponseEntity.ok().build();
    }
}