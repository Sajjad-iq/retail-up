package com.sajjadkademm.retail.settings.system.controller;

import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsResponse;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/settings/system")
@RequiredArgsConstructor
public class SystemSettingsController {

    private final SystemSettingsService systemSettingsService;

    /**
     * Get all system settings for an organization
     */
    @GetMapping("/organization/{organizationId}")
    public ResponseEntity<List<SystemSettingsResponse>> getSystemSettings(@PathVariable String organizationId) {
        List<SystemSettingsResponse> response = systemSettingsService.getSystemSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get specific system setting by key
     */
    @GetMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<SystemSettingsResponse> getSystemSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        SystemSettingsResponse response = systemSettingsService.getSystemSettingByKey(organizationId, key);
        return ResponseEntity.ok(response);
    }

    /**
     * Create or update system setting
     */
    @PostMapping("/organization/{organizationId}")
    public ResponseEntity<SystemSettingsResponse> createOrUpdateSystemSetting(
            @PathVariable String organizationId,
            @Valid @RequestBody SystemSettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        SystemSettingsResponse response = systemSettingsService.createOrUpdateSystemSetting(organizationId, request,
                userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update system setting by key
     */
    @PutMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<SystemSettingsResponse> updateSystemSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key,
            @Valid @RequestBody SystemSettingsRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        SystemSettingsResponse response = systemSettingsService.updateSystemSettingByKey(organizationId, key, request,
                userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Delete system setting by key
     */
    @DeleteMapping("/organization/{organizationId}/key/{key}")
    public ResponseEntity<Void> deleteSystemSettingByKey(
            @PathVariable String organizationId,
            @PathVariable String key) {
        systemSettingsService.deleteSystemSettingByKey(organizationId, key);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get system settings by category (e.g., "security", "performance", "backup")
     */
    @GetMapping("/organization/{organizationId}/category/{category}")
    public ResponseEntity<List<SystemSettingsResponse>> getSystemSettingsByCategory(
            @PathVariable String organizationId,
            @PathVariable String category) {
        List<SystemSettingsResponse> response = systemSettingsService.getSystemSettingsByCategory(organizationId,
                category);
        return ResponseEntity.ok(response);
    }

    /**
     * Get default system settings for an organization
     */
    @GetMapping("/organization/{organizationId}/defaults")
    public ResponseEntity<List<SystemSettingsResponse>> getDefaultSystemSettings(@PathVariable String organizationId) {
        List<SystemSettingsResponse> response = systemSettingsService.getDefaultSystemSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Initialize default system settings for a new organization
     */
    @PostMapping("/organization/{organizationId}/initialize")
    public ResponseEntity<Void> initializeDefaultSystemSettings(@PathVariable String organizationId) {
        systemSettingsService.initializeDefaultSettings(organizationId);
        return ResponseEntity.ok().build();
    }
}