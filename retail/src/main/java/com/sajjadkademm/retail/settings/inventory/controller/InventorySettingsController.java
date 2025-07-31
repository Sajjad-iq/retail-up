package com.sajjadkademm.retail.settings.inventory.controller;

import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/settings/inventory")
@RequiredArgsConstructor
public class InventorySettingsController {

    private final InventorySettingsService inventorySettingsService;

    /**
     * Get inventory settings for an organization
     */
    @GetMapping("/organization/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> getInventorySettings(@PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.getInventorySettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Create or update inventory settings
     */
    @PostMapping("/organization/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> createOrUpdateInventorySettings(
            @PathVariable String organizationId,
            @Valid @RequestBody InventorySettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        InventorySettingsResponse response = inventorySettingsService.createOrUpdateInventorySettings(organizationId,
                request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update inventory settings
     */
    @PutMapping("/organization/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> updateInventorySettings(
            @PathVariable String organizationId,
            @Valid @RequestBody InventorySettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        InventorySettingsResponse response = inventorySettingsService.updateInventorySettings(organizationId, request,
                userId);
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

    /**
     * Reset inventory settings to defaults
     */
    @PostMapping("/organization/{organizationId}/reset")
    public ResponseEntity<InventorySettingsResponse> resetInventorySettingsToDefaults(
            @PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}