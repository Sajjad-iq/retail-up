package com.sajjadkademm.retail.inventory.ExcelUpload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * Request DTO for Excel upload operations containing the file and inventory
 * context
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ExcelUploadRequest {

    /**
     * Inventory ID where the items will be created
     */
    @NotBlank(message = "Inventory ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "Inventory ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "Inventory ID must be between 20 and 255 characters")
    private String inventoryId;

    /**
     * User ID performing the upload operation
     */
    @NotBlank(message = "User ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "User ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "User ID must be between 20 and 255 characters")
    private String userId;
}