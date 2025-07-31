package com.sajjadkademm.retail.settings.pos.controller;

import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsResponse;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/settings/pos")
@RequiredArgsConstructor
public class POSSettingsController {

    private final POSSettingsService posSettingsService;

    /**
     * Get all POS settings for an organization
     */
    @GetMapping("/organization/{organizationId}")
    public ResponseEntity<List<POSSettingsResponse>> getPOSSettings(@PathVariable String organizationId) {
        List<POSSettingsResponse> response = posSettingsService.getPOSSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get specific POS setting by key
     */
    @GetMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<POSSettingsResponse> getPOSSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        POSSettingsResponse response = posSettingsService.getPOSSettingByKey(organizationId, key);
        return ResponseEntity.ok(response);
    }

    /**
     * Create or update POS setting
     */
    @PostMapping("/organization/{organizationId}")
    public ResponseEntity<POSSettingsResponse> createOrUpdatePOSSetting(
            @PathVariable String organizationId,
            @Valid @RequestBody POSSettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        POSSettingsResponse response = posSettingsService.createOrUpdatePOSSetting(organizationId, request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update POS setting by key
     */
    @PutMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<POSSettingsResponse> updatePOSSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key,
            @Valid @RequestBody POSSettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        POSSettingsResponse response = posSettingsService.updatePOSSettingByKey(organizationId, key, request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Delete POS setting by key
     */
    @DeleteMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<Void> deletePOSSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        posSettingsService.deletePOSSettingByKey(organizationId, key);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get POS settings by category (e.g., "payment", "receipt", "tax")
     */
    @GetMapping("/organization/{organizationId}/category/{category}")
    public ResponseEntity<List<POSSettingsResponse>> getPOSSettingsByCategory(
            @PathVariable String organizationId,
            @PathVariable String category) {
        List<POSSettingsResponse> response = posSettingsService.getPOSSettingsByCategory(organizationId, category);
        return ResponseEntity.ok(response);
    }

    /**
     * Get payment methods configuration
     */
    @GetMapping("/organization/{organizationId}/payment-methods")
    public ResponseEntity<List<POSSettingsResponse>> getPaymentMethods(@PathVariable String organizationId) {
        List<POSSettingsResponse> response = posSettingsService.getPaymentMethods(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get tax configuration
     */
    @GetMapping("/organization/{organizationId}/tax-config")
    public ResponseEntity<List<POSSettingsResponse>> getTaxConfig(@PathVariable String organizationId) {
        List<POSSettingsResponse> response = posSettingsService.getTaxConfig(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get default POS settings for an organization
     */
    @GetMapping("/organization/{organizationId}/defaults")
    public ResponseEntity<List<POSSettingsResponse>> getDefaultPOSSettings(@PathVariable String organizationId) {
        List<POSSettingsResponse> response = posSettingsService.getDefaultPOSSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Initialize default POS settings for a new organization
     */
    @PostMapping("/organization/{organizationId}/initialize")
    public ResponseEntity<Void> initializeDefaultPOSSettings(@PathVariable String organizationId) {
        posSettingsService.initializeDefaultSettings(organizationId);
        return ResponseEntity.ok().build();
    }
}