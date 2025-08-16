package com.sajjadkademm.retail.inventory.ExcelUpload.utils;

import org.springframework.web.multipart.MultipartFile;

/**
 * Utility class for Excel upload operations
 */
public class ExcelUploadUtils {

    /**
     * Validate if the uploaded file is a valid CSV file
     */
    public static boolean isValidCsvFile(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            return false;
        }

        String fileName = file.getOriginalFilename();
        if (fileName == null) {
            return false;
        }

        // Accept CSV files
        return fileName.toLowerCase().endsWith(".csv");
    }

    /**
     * Get the expected column headers for CSV upload
     */
    public static String[] getExpectedHeaders() {
        return new String[] {
                "Name", "Description", "SKU", "Product Code", "Barcode",
                "Category", "Brand", "Unit", "Weight", "Dimensions",
                "Color", "Size", "Current Stock", "Minimum Stock", "Maximum Stock",
                "Cost Price Amount", "Cost Price Currency", "Selling Price Amount",
                "Selling Price Currency", "Discount Price", "Discount Start Date",
                "Discount End Date", "Supplier Name", "Is Perishable", "Expiry Date"
        };
    }

    /**
     * Generate CSV template content
     */
    public static String generateCsvTemplate() {
        StringBuilder template = new StringBuilder();
        String[] headers = getExpectedHeaders();

        for (int i = 0; i < headers.length; i++) {
            template.append(headers[i]);
            if (i < headers.length - 1) {
                template.append(",");
            }
        }
        template.append("\n");

        // Add example row
        template.append(
                "Sample Product,Product description,SAMPLE001,PROD001,123456789,Electronics,Brand Name,PIECES,100.5,10x5x2,Black,Medium,50,10,100,25.00,USD,29.99,USD,24.99,2024-01-01,2024-12-31,Supplier Inc,false,2025-12-31");

        return template.toString();
    }
}