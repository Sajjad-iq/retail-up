package com.sajjadkademm.retail.inventory.ExcelUpload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.NotBlank;
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
    @NotBlank(message = "inventory.id.required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "inventory.id.invalid")
    @Size(min = 20, max = 255, message = "inventory.id.invalid")
    private String inventoryId;

    /**
     * User ID performing the upload operation
     */
    @NotBlank(message = "user.id.required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "user.id.invalid")
    @Size(min = 20, max = 255, message = "user.id.invalid")
    private String userId;
}