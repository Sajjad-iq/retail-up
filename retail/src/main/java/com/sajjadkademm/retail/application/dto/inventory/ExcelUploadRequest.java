package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
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
    @Pattern(regexp = ValidationConstants.ID_PATTERN, message = "inventory.id.invalid")
    @Size(min = ValidationConstants.MIN_ID_LENGTH, max = ValidationConstants.MAX_ID_LENGTH, message = "inventory.id.invalid")
    private String inventoryId;

    /**
     * User ID performing the upload operation
     */
    @NotBlank(message = "user.id.required")
    @Pattern(regexp = ValidationConstants.ID_PATTERN, message = "user.id.invalid")
    @Size(min = ValidationConstants.MIN_ID_LENGTH, max = ValidationConstants.MAX_ID_LENGTH, message = "user.id.invalid")
    private String userId;
}