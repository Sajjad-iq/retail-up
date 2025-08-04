package com.sajjadkademm.retail.settings.system.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.system.dto.SystemSettingsRequest;
import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;

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
import io.swagger.v3.oas.annotations.responses.ApiResponses;
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
        @ApiResponses(value = {
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
                                        """))),
                        @ApiResponse(responseCode = "404", description = "System settings not found for organization", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
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
        @ApiResponses(value = {
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
                                        """))),
                        @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
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
        @ApiResponses(value = {
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
                                        """))),
                        @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @PostMapping("/{organizationId}/reset")
        public ResponseEntity<SystemSetting> resetSystemSettingsToDefaults(
                        @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
                SystemSetting response = systemSettingsService.resetToDefaults(organizationId);
                return ResponseEntity.ok(response);
        }
}