package com.sajjadkademm.retail.settings.inventory.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/settings/inventory")
@RequiredArgsConstructor
public class InventorySettingsController {

    private final InventorySettingsService inventorySettingsService;
    private final JwtUtil jwtUtil;

    /**
     * Get inventory settings for an organization
     */
    @GetMapping("/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> getInventorySettings(@PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.getInventorySettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update inventory settings
     */
    @PutMapping("/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> updateInventorySettings(
            @PathVariable String organizationId,
            @Valid @RequestBody InventorySettingsRequest request,
            HttpServletRequest httpRequest) {

        // Extract user ID from JWT token
        String authHeader = httpRequest.getHeader("Authorization");
        String userId = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            userId = jwtUtil.extractUserId(token);
        }

        InventorySettingsResponse response = inventorySettingsService.updateInventorySettings(organizationId, request,
                userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset inventory settings to defaults
     */
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<InventorySettingsResponse> resetInventorySettingsToDefaults(
            @PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}