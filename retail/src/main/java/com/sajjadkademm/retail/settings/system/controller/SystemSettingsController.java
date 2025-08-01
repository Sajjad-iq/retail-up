package com.sajjadkademm.retail.settings.system.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsResponse;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/settings/system")
@RequiredArgsConstructor
public class SystemSettingsController {

    private final SystemSettingsService systemSettingsService;
    private final JwtUtil jwtUtil;

    /**
     * Get system settings for an organization
     */
    @GetMapping("/{organizationId}")
    public ResponseEntity<SystemSettingsResponse> getSystemSettings(@PathVariable String organizationId) {
        SystemSettingsResponse response = systemSettingsService.getSystemSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update system settings
     */
    @PutMapping("/{organizationId}")
    public ResponseEntity<SystemSettingsResponse> updateSystemSettings(
            @PathVariable String organizationId,
            @Valid @RequestBody SystemSettingsRequest request,
            HttpServletRequest httpRequest) {

        // Extract user ID from JWT token
        String authHeader = httpRequest.getHeader("Authorization");
        String userId = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            userId = jwtUtil.extractUserId(token);
        }

        SystemSettingsResponse response = systemSettingsService.updateSystemSettings(organizationId, request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset system settings to defaults
     */
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<SystemSettingsResponse> resetSystemSettingsToDefaults(@PathVariable String organizationId) {
        SystemSettingsResponse response = systemSettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}