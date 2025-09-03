package com.sajjadkademm.retail.application.controllers.inventory;

import com.sajjadkademm.retail.inventory.ExcelUpload.ExcelUploadService;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.application.dto.inventory.ExcelUploadResponse;
import com.sajjadkademm.retail.inventory.ExcelUpload.utils.ExcelUploadUtils;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.users.UserService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * REST controller for handling CSV file uploads to create inventory items in
 * bulk.
 * Provides endpoints for uploading CSV files and processing them to create
 * inventory items.
 */
@RestController
@RequestMapping("/api/v1/inventory/csv-upload")
@Tag(name = "CSV Upload", description = "Endpoints for bulk uploading inventory items via CSV files")
public class ExcelUploadController {

    private final ExcelUploadService excelUploadService;
    private final UserService userService;

    @Autowired
    public ExcelUploadController(ExcelUploadService excelUploadService, UserService userService) {
        this.excelUploadService = excelUploadService;
        this.userService = userService;
    }

    /**
     * Upload CSV file to create inventory items in bulk
     */
    @PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Upload CSV file to create inventory items", description = "Processes a CSV file and creates inventory items in bulk for the specified inventory")
    public ResponseEntity<ExcelUploadResponse> uploadCsvFile(
            @Parameter(description = "CSV file to upload") @RequestParam("file") MultipartFile file,
            @Parameter(description = "Inventory ID where items will be created") @RequestParam("inventoryId") String inventoryId,
            @Parameter(description = "User ID performing the upload") @RequestParam("userId") String userId) {

        try {
            // Validate file
            if (file.isEmpty()) {
                throw new BadRequestException("File is empty");
            }

            // Validate file type
            if (!ExcelUploadUtils.isValidCsvFile(file)) {
                throw new BadRequestException("Invalid file type");
            }

            // Get user
            User user = userService.getUserById(userId);

            // Process file
            ExcelUploadResponse response = excelUploadService.processExcelFile(file, inventoryId, user);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            throw new BadRequestException(e.getMessage());
        }
    }

    /**
     * Get CSV template for inventory items
     */
    @GetMapping("/template")
    @Operation(summary = "Download CSV template", description = "Downloads a CSV template with the correct column structure for bulk inventory item creation")
    public ResponseEntity<String> getTemplate() {
        String template = ExcelUploadUtils.generateCsvTemplate();

        return ResponseEntity.ok()
                .header("Content-Type", "text/csv")
                .header("Content-Disposition", "attachment; filename=\"inventory_items_template.csv\"")
                .body(template);
    }
}