package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import java.util.List;

/**
 * Response DTO for Excel upload operations containing summary of the upload
 * process
 * and details about successful and failed items.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ExcelUploadResponse {

    /**
     * Total number of rows processed from the Excel file
     */
    private Integer totalRows;

    /**
     * Number of items successfully created
     */
    private Integer successfulItems;

    /**
     * Number of items that failed to create
     */
    private Integer failedItems;

    /**
     * List of successfully created inventory items
     */
    private List<InventoryItem> createdItems;

    /**
     * List of error messages for failed items
     */
    private List<String> errors;
}