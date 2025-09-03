package com.sajjadkademm.retail.application.controllers.settings;

import com.sajjadkademm.retail.application.services.settings.SystemSettingsService;
import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.application.dto.settings.SystemSettingsRequest;
import com.sajjadkademm.retail.domain.settings.model.SystemSetting;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

/**
 * System settings controller for managing organization-wide system
 * configurations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/settings/system")
@RequiredArgsConstructor
@Tag(name = "System Settings", description = "System settings management endpoints")
public class SystemSettingsController {

    private final SystemSettingsService systemSettingsService;

    /**
     * Get system settings for an organization
     */
    @Operation(summary = "Get System Settings", description = "Retrieve system settings for a specific organization", operationId = "getSystemSettings")
    @ApiResponse(responseCode = "200", description = "System settings retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SystemSetting.class), examples = @ExampleObject(name = "System Settings", value = """
            {
                "id": "sys123",
                "organizationId": "org123",
                "twoFactorAuthEnabled": true,
                "autoBackupEnabled": true,
                "backupRetentionDays": 30,
                "timezone": "America/New_York",
                "language": "en",
                "currency": "USD",
                "emailNotificationsEnabled": true,
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T10:30:00"
            }
            """)))
    @GetMapping("/{organizationId}")
    public ResponseEntity<SystemSetting> getSystemSettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        SystemSetting response = systemSettingsService.getSystemSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update system settings
     */
    @Operation(summary = "Update System Settings", description = "Update system settings for a specific organization", operationId = "updateSystemSettings", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponse(responseCode = "200", description = "System settings updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SystemSetting.class), examples = @ExampleObject(name = "Updated System Settings", value = """
            {
                "id": "sys123",
                "organizationId": "org123",
                "twoFactorAuthEnabled": false,
                "autoBackupEnabled": true,
                "backupRetentionDays": 60,
                "timezone": "Europe/London",
                "language": "en",
                "currency": "EUR",
                "emailNotificationsEnabled": false,
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T11:30:00"
            }
            """)))
    @PutMapping("/{organizationId}")
    public ResponseEntity<SystemSetting> updateSystemSettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId,
            @Parameter(description = "System settings update request", required = true, content = @Content(schema = @Schema(implementation = SystemSettingsRequest.class), examples = @ExampleObject(name = "Update System Settings Request", value = """
                    {
                        "twoFactorAuthEnabled": false,
                        "autoBackupEnabled": true,
                        "backupRetentionDays": 60,
                        "timezone": "Europe/London",
                        "language": "en",
                        "currency": "EUR",
                        "emailNotificationsEnabled": false
                    }
                    """))) @Valid @RequestBody SystemSettingsRequest request) {

        SystemSetting response = systemSettingsService.updateSystemSettings(organizationId, request);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset system settings to defaults
     */
    @Operation(summary = "Reset System Settings to Defaults", description = "Reset system settings to default values for a specific organization", operationId = "resetSystemSettingsToDefaults", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponse(responseCode = "200", description = "System settings reset to defaults successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = SystemSetting.class), examples = @ExampleObject(name = "Reset System Settings", value = """
            {
                "id": "sys123",
                "organizationId": "org123",
                "twoFactorAuthEnabled": true,
                "autoBackupEnabled": true,
                "backupRetentionDays": 30,
                "timezone": "UTC",
                "language": "en",
                "currency": "USD",
                "emailNotificationsEnabled": true,
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T12:30:00"
            }
            """)))
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<SystemSetting> resetSystemSettingsToDefaults(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        SystemSetting response = systemSettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}