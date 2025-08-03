package com.sajjadkademm.retail.settings.pos.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.pos.dto.POSSettingsRequest;
import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;

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
 * POS settings controller for managing Point of Sale system configurations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/settings/pos")
@RequiredArgsConstructor
@Tag(name = "POS Settings", description = "Point of Sale settings management endpoints")
public class POSSettingsController {

    private final POSSettingsService posSettingsService;
    private final JwtUtil jwtUtil;

    /**
     * Get POS settings for an organization
     */
    @Operation(summary = "Get POS Settings", description = "Retrieve POS settings for a specific organization", operationId = "getPOSSettings")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "POS settings retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = POSSetting.class), examples = @ExampleObject(name = "POS Settings", value = """
                    {
                        "id": "pos123",
                        "organizationId": "org123",
                        "cashPaymentEnabled": true,
                        "cardPaymentEnabled": true,
                        "changeCalculationMethod": "automatic",
                        "allowPartialPayments": false,
                        "requireExactChange": true,
                        "autoPrintReceipts": true,
                        "receiptTemplateHtml": "templates/receipt-main.html",
                        "receiptHeaderTemplateHtml": "templates/receipt-header.html",
                        "receiptFooterTemplateHtml": "templates/receipt-footer.html",
                        "receiptPaperWidthMm": 80,
                        "holdTransactionsEnabled": true,
                        "returnsEnabled": true,
                        "discountsEnabled": true,
                        "customerLookupEnabled": true,
                        "requireCustomerInfo": false,
                        "showProductImages": true,
                        "showStockLevels": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T10:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "POS settings not found for organization", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @GetMapping("/{organizationId}")
    public ResponseEntity<POSSetting> getPOSSettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        POSSetting response = posSettingsService.getPOSSettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update POS settings
     */
    @Operation(summary = "Update POS Settings", description = "Update POS settings for a specific organization", operationId = "updatePOSSettings", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "POS settings updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = POSSetting.class), examples = @ExampleObject(name = "Updated POS Settings", value = """
                    {
                        "id": "pos123",
                        "organizationId": "org123",
                        "cashPaymentEnabled": true,
                        "cardPaymentEnabled": true,
                        "changeCalculationMethod": "manual",
                        "allowPartialPayments": true,
                        "requireExactChange": false,
                        "autoPrintReceipts": false,
                        "receiptTemplateHtml": "templates/updated-receipt-main.html",
                        "receiptHeaderTemplateHtml": "templates/updated-receipt-header.html",
                        "receiptFooterTemplateHtml": "templates/updated-receipt-footer.html",
                        "receiptPaperWidthMm": 58,
                        "holdTransactionsEnabled": false,
                        "returnsEnabled": true,
                        "discountsEnabled": false,
                        "customerLookupEnabled": false,
                        "requireCustomerInfo": true,
                        "showProductImages": false,
                        "showStockLevels": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T11:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PutMapping("/{organizationId}")
    public ResponseEntity<POSSetting> updatePOSSettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId,
            @Parameter(description = "POS settings update request", required = true, content = @Content(schema = @Schema(implementation = POSSettingsRequest.class), examples = @ExampleObject(name = "Update POS Settings Request", value = """
                    {
                        "userId": "user123",
                        "cashPaymentEnabled": true,
                        "cardPaymentEnabled": true,
                        "changeCalculationMethod": "manual",
                        "allowPartialPayments": true,
                        "requireExactChange": false,
                        "autoPrintReceipts": false,
                        "receiptTemplateHtml": "templates/updated-receipt-main.html",
                        "receiptHeaderTemplateHtml": "templates/updated-receipt-header.html",
                        "receiptFooterTemplateHtml": "templates/updated-receipt-footer.html",
                        "receiptPaperWidthMm": 58,
                        "holdTransactionsEnabled": false,
                        "returnsEnabled": true,
                        "discountsEnabled": false,
                        "customerLookupEnabled": false,
                        "requireCustomerInfo": true,
                        "showProductImages": false,
                        "showStockLevels": true
                    }
                    """))) @Valid @RequestBody POSSettingsRequest request) {

        POSSetting response = posSettingsService.updatePOSSettings(organizationId, request);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset POS settings to defaults
     */
    @Operation(summary = "Reset POS Settings to Defaults", description = "Reset POS settings to default values for a specific organization", operationId = "resetPOSSettingsToDefaults", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "POS settings reset to defaults successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = POSSetting.class), examples = @ExampleObject(name = "Reset POS Settings", value = """
                    {
                        "id": "pos123",
                        "organizationId": "org123",
                        "cashPaymentEnabled": true,
                        "cardPaymentEnabled": true,
                        "changeCalculationMethod": "automatic",
                        "allowPartialPayments": false,
                        "requireExactChange": false,
                        "autoPrintReceipts": true,
                        "receiptTemplateHtml": "templates/receipt-main.html",
                        "receiptHeaderTemplateHtml": "templates/receipt-header.html",
                        "receiptFooterTemplateHtml": "templates/receipt-footer.html",
                        "receiptPaperWidthMm": 80,
                        "holdTransactionsEnabled": true,
                        "returnsEnabled": true,
                        "discountsEnabled": true,
                        "customerLookupEnabled": true,
                        "requireCustomerInfo": false,
                        "showProductImages": true,
                        "showStockLevels": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T12:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<POSSetting> resetPOSSettingsToDefaults(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        POSSetting response = posSettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}