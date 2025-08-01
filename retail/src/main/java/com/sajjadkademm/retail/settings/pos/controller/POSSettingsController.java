package com.sajjadkademm.retail.settings.pos.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsResponse;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/settings/pos")
@RequiredArgsConstructor
public class POSSettingsController {

    private final POSSettingsService posSettingsService;
    private final JwtUtil jwtUtil;

    /**
     * Get POS settings for an organization
     */
    @GetMapping("/{organizationId}")
    public ResponseEntity<POSSettingsResponse> getPOSSettings(@PathVariable String organizationId) {
        POSSettingsResponse response = posSettingsService.getPOSSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update POS settings
     */
    @PutMapping("/{organizationId}")
    public ResponseEntity<POSSettingsResponse> updatePOSSettings(
            @PathVariable String organizationId,
            @Valid @RequestBody POSSettingsRequest request,
            HttpServletRequest httpRequest) {

        // Extract user ID from JWT token
        String authHeader = httpRequest.getHeader("Authorization");
        String userId = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            userId = jwtUtil.extractUserId(token);
        }

        POSSettingsResponse response = posSettingsService.updatePOSSettings(organizationId, request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset POS settings to defaults
     */
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<POSSettingsResponse> resetPOSSettingsToDefaults(@PathVariable String organizationId) {
        POSSettingsResponse response = posSettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}