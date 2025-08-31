package com.sajjadkademm.retail.inventory.ExcelUpload;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemService;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.shared.enums.Money;
import com.sajjadkademm.retail.shared.enums.Unit;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemUpdateValidator;
import com.sajjadkademm.retail.inventory.InventoryItem.validator.InventoryItemValidationUtils.ValidationResult;
import com.sajjadkademm.retail.inventory.InventoryMovement.InventoryMovementService;
import com.sajjadkademm.retail.inventory.ExcelUpload.dto.ExcelUploadResponse;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemResult;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.shared.enums.Currency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Service for handling Excel file uploads to create inventory items in bulk.
 * Processes Excel files and creates inventory items using the existing
 * InventoryItemService.
 */
@Service
public class ExcelUploadService {
    private final InventoryItemService inventoryItemService;
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemUpdateValidator inventoryItemUpdateValidator;
    private final InventoryMovementService inventoryMovementService;

    @Autowired
    public ExcelUploadService(InventoryItemService inventoryItemService,
            InventoryItemRepository inventoryItemRepository,
            InventoryItemUpdateValidator inventoryItemUpdateValidator,
            InventoryMovementService inventoryMovementService) {
        this.inventoryItemService = inventoryItemService;
        this.inventoryItemRepository = inventoryItemRepository;
        this.inventoryItemUpdateValidator = inventoryItemUpdateValidator;
        this.inventoryMovementService = inventoryMovementService;
    }

    /**
     * Process Excel file and batch create/update inventory items.
     * This method handles the entire Excel upload workflow including parsing,
     * validation, creation/updating of items, and error collection.
     * 
     * @param file        The uploaded Excel/CSV file
     * @param inventoryId The target inventory ID for the items
     * @param user        The user performing the upload operation
     * @return ExcelUploadResponse with summary of results and any errors
     * @throws BadRequestException if file processing fails
     */
    @Transactional(rollbackFor = { BadRequestException.class })
    public ExcelUploadResponse processExcelFile(MultipartFile file, String inventoryId, User user) {
        try {
            // Parse the Excel/CSV file into inventory item requests
            List<CreateInventoryItemRequest> items = parseExcelFile(file, inventoryId);

            if (items.isEmpty()) {
                throw new BadRequestException(
                        "No valid rows found in the CSV file. Please check the file format and ensure it contains data.");
            }

            // Collections to track processing results
            List<InventoryItem> processedItems = new ArrayList<>();
            List<String> processingErrors = new ArrayList<>();

            // Process each item from the Excel file
            for (int i = 0; i < items.size(); i++) {
                CreateInventoryItemRequest itemRequest = items.get(i);

                int rowNumber = i + 2; // CSV row number (accounting for header row)

                // Check if item already exists (by barcode or product code)
                Optional<InventoryItem> existingItem = findExistingItemByIdentifiers(itemRequest, inventoryId);

                // Process item: create new or update existing
                processInventoryItem(existingItem, itemRequest, user, rowNumber, processedItems, processingErrors);
            }

            // Build and return response with processing summary
            return ExcelUploadResponse.builder()
                    .totalRows(items.size())
                    .successfulItems(processedItems.size())
                    .failedItems(processingErrors.size())
                    .createdItems(processedItems)
                    .errors(processingErrors)
                    .build();

        } catch (Exception e) {
            throw new BadRequestException("Failed to process Excel file: " + e.getMessage(), e);
        }
    }

    /**
     * Parse CSV file and convert to CreateInventoryItemRequest objects
     */
    private List<CreateInventoryItemRequest> parseExcelFile(MultipartFile file, String inventoryId) throws IOException {
        List<CreateInventoryItemRequest> items = new ArrayList<>();
        final int EXPECTED_COLUMNS = 25; // Based on the CSV template structure

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream()))) {
            String line;
            boolean isFirstLine = true;

            while ((line = reader.readLine()) != null) {

                if (isFirstLine) {
                    isFirstLine = false;
                    continue; // Skip header row
                }

                if (line.trim().isEmpty()) {
                    continue;
                }

                String[] values = parseCsvLine(line);

                // Validate column count
                if (values.length < EXPECTED_COLUMNS) {
                    String[] paddedValues = new String[EXPECTED_COLUMNS];
                    System.arraycopy(values, 0, paddedValues, 0, values.length);
                    for (int j = values.length; j < EXPECTED_COLUMNS; j++) {
                        paddedValues[j] = "";
                    }
                    values = paddedValues;
                } else if (values.length > EXPECTED_COLUMNS) {
                    String[] truncatedValues = new String[EXPECTED_COLUMNS];
                    System.arraycopy(values, 0, truncatedValues, 0, EXPECTED_COLUMNS);
                    values = truncatedValues;
                }

                CreateInventoryItemRequest item = parseRow(values, inventoryId);
                if (item != null) {
                    items.add(item);
                }
            }
        }

        return items;
    }

    /**
     * Parse a CSV line, handling quoted fields and commas within fields
     */
    private String[] parseCsvLine(String line) {
        List<String> result = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);

            if (c == '"') {
                inQuotes = !inQuotes;
            } else if (c == ',' && !inQuotes) {
                result.add(current.toString().trim());
                current = new StringBuilder();
            } else {
                current.append(c);
            }
        }

        // Add the last field
        result.add(current.toString().trim());

        return result.toArray(new String[0]);
    }

    /**
     * Parse a single row from CSV and convert to CreateInventoryItemRequest
     */
    private CreateInventoryItemRequest parseRow(String[] values, String inventoryId) {
        try {
            // Extract and validate required fields using validator utilities
            String name = getStringValue(values, 0);
            String unitStr = getStringValue(values, 7);
            String currentStockStr = getStringValue(values, 12);
            String sellingPriceAmountStr = getStringValue(values, 17);
            String sellingPriceCurrencyStr = getStringValue(values, 18);

            // Note: Detailed validation will be done by validator utilities later

            CreateInventoryItemRequest item = new CreateInventoryItemRequest();
            item.setName(name);
            item.setDescription(getStringValue(values, 1));

            item.setProductCode(getStringValue(values, 3));
            item.setBarcode(getStringValue(values, 4));
            item.setCategory(getStringValue(values, 5));
            item.setBrand(getStringValue(values, 6));
            item.setUnit(parseUnit(unitStr));
            item.setWeight(parseBigDecimal(getStringValue(values, 8)));
            item.setDimensions(getStringValue(values, 9));
            item.setColor(getStringValue(values, 10));
            item.setSize(getStringValue(values, 11));
            item.setCurrentStock(parseInteger(currentStockStr));
            item.setMinimumStock(parseInteger(getStringValue(values, 13)));
            item.setMaximumStock(parseInteger(getStringValue(values, 14)));
            item.setCostPrice(parseMoney(getStringValue(values, 15), getStringValue(values, 16)));
            item.setSellingPrice(parseMoney(sellingPriceAmountStr, sellingPriceCurrencyStr));
            item.setDiscountPrice(parseBigDecimal(getStringValue(values, 19)));
            item.setDiscountStartDate(parseDateTime(getStringValue(values, 20)));
            item.setDiscountEndDate(parseDateTime(getStringValue(values, 21)));
            item.setSupplierName(getStringValue(values, 22));
            item.setIsPerishable(parseBoolean(getStringValue(values, 23)));

            // Handle expiry date - let validator utilities handle null/empty validation
            item.setExpiryDate(parseDate(getStringValue(values, 24)));

            item.setInventoryId(inventoryId);

            return item;
        } catch (Exception e) {
            // Return null for invalid rows, they will be counted as errors
            return null;
        }
    }

    /**
     * Find existing inventory item by unique identifiers (barcode or product
     * code).
     * This method checks for duplicates before creating new items during Excel
     * upload.
     * Priority order: Barcode -> Product Code
     * 
     * @param itemRequest The item request containing potential identifiers
     * @param inventoryId The inventory to search within
     * @return Optional containing existing item if found, empty otherwise
     */
    private Optional<InventoryItem> findExistingItemByIdentifiers(CreateInventoryItemRequest itemRequest,
            String inventoryId) {

        // Check by barcode
        if (itemRequest.getBarcode() != null && !itemRequest.getBarcode().trim().isEmpty()) {
            Optional<InventoryItem> existing = inventoryItemRepository
                    .findByBarcodeAndInventoryId(itemRequest.getBarcode(), inventoryId);
            if (existing.isPresent())
                return existing;
        }

        // Check by product code
        if (itemRequest.getProductCode() != null && !itemRequest.getProductCode().trim().isEmpty()) {
            return inventoryItemRepository.findByProductCodeAndInventoryId(itemRequest.getProductCode(), inventoryId);
        }

        return Optional.empty();
    }

    /**
     * Process an inventory item from Excel upload by either creating new or
     * updating existing item.
     * Determines whether to create or update based on finding existing items by
     * barcode or product code.
     * 
     * @param existingItem Optional existing item found by unique identifiers
     * @param itemRequest  The item data from Excel row
     * @param user         The user performing the upload
     * @param rowNumber    The Excel row number for error reporting
     * @param createdItems List to collect successfully processed items
     * @param errors       List to collect error messages for failed items
     */
    private void processInventoryItem(Optional<InventoryItem> existingItem, CreateInventoryItemRequest itemRequest,
            User user, int rowNumber, List<InventoryItem> createdItems, List<String> errors) {
        try {
            if (existingItem.isPresent()) {
                updateExistingItem(existingItem.get(), itemRequest, user, rowNumber, createdItems, errors);
            } else {
                createNewItem(itemRequest, user, rowNumber, createdItems, errors);
            }
        } catch (Exception e) {
            errors.add("Row " + rowNumber + ": Unexpected error - " + e.getMessage());
        }
    }

    /**
     * Update existing inventory item found during Excel upload.
     * Converts the create request to an update request and applies validation and
     * updates.
     * 
     * @param existingItem The existing inventory item to update
     * @param itemRequest  The new item data from Excel row
     * @param user         The user performing the upload
     * @param rowNumber    The Excel row number for error reporting
     * @param createdItems List to add successfully updated items
     * @param errors       List to add error messages for failed updates
     */
    private void updateExistingItem(InventoryItem existingItem, CreateInventoryItemRequest itemRequest, User user,
            int rowNumber, List<InventoryItem> createdItems, List<String> errors) {
        // Convert create request to update request for existing item
        UpdateInventoryItemRequest updateRequest = convertCreateToUpdateRequest(itemRequest, user.getId());

        // Validate update request and collect all errors without throwing exceptions
        ValidationResult validationResult = inventoryItemUpdateValidator
                .validateAndCollectErrors(existingItem, updateRequest);

        if (validationResult.hasErrors()) {
            // Collect validation errors for this row
            String errorMessage = String.join("; ", validationResult.getErrors());
            errors.add("Row " + rowNumber + ": " + errorMessage);
        } else {
            // Apply updates if validation passes
            Integer originalStock = existingItem.getCurrentStock();
            inventoryItemUpdateValidator.applyUpdates(existingItem, updateRequest);
            InventoryItem updated = inventoryItemRepository.save(existingItem);

            // Track inventory movements for audit trail
            inventoryItemUpdateValidator.trackStockMovements(updated, updateRequest, inventoryMovementService,
                    originalStock);
            createdItems.add(updated);
        }
    }

    /**
     * Create new inventory item for Excel batch processing.
     * Uses error collection approach to avoid stopping the entire batch on first
     * error.
     * 
     * @param itemRequest  The inventory item creation request from Excel row
     * @param user         The user performing the upload
     * @param rowNumber    The row number in Excel for error reporting
     * @param createdItems List to add successfully created items
     * @param errors       List to add error messages for failed items
     */
    private void createNewItem(CreateInventoryItemRequest itemRequest, User user, int rowNumber,
            List<InventoryItem> createdItems, List<String> errors) {
        // Use inventory service method designed for batch operations with error
        // collection
        CreateInventoryItemResult result = inventoryItemService.createInventoryItemWithErrorCollection(itemRequest);

        if (result.isSuccess()) {
            createdItems.add(result.getItem());
        } else {
            errors.add("Row " + rowNumber + ": " + result.getErrorMessage());
        }
    }

    /**
     * Convert CreateInventoryItemRequest to UpdateInventoryItemRequest.
     * This mapping is needed when we find an existing item during Excel upload
     * and need to update it with the new data from the Excel row.
     * 
     * @param createRequest The original creation request from Excel row
     * @param userId        The ID of the user performing the operation
     * @return UpdateInventoryItemRequest with mapped data
     */
    private UpdateInventoryItemRequest convertCreateToUpdateRequest(CreateInventoryItemRequest createRequest,
            String userId) {
        UpdateInventoryItemRequest updateRequest = new UpdateInventoryItemRequest();
        updateRequest.setUserId(userId);
        updateRequest.setName(createRequest.getName());
        updateRequest.setDescription(createRequest.getDescription());

        updateRequest.setProductCode(createRequest.getProductCode());
        updateRequest.setBarcode(createRequest.getBarcode());
        updateRequest.setCategory(createRequest.getCategory());
        updateRequest.setBrand(createRequest.getBrand());
        updateRequest.setUnit(createRequest.getUnit());
        updateRequest.setWeight(createRequest.getWeight());
        updateRequest.setDimensions(createRequest.getDimensions());
        updateRequest.setColor(createRequest.getColor());
        updateRequest.setSize(createRequest.getSize());
        updateRequest.setCurrentStock(createRequest.getCurrentStock());
        updateRequest.setMinimumStock(createRequest.getMinimumStock());
        updateRequest.setMaximumStock(createRequest.getMaximumStock());
        updateRequest.setCostPrice(createRequest.getCostPrice());
        updateRequest.setSellingPrice(createRequest.getSellingPrice());
        updateRequest.setDiscountPrice(createRequest.getDiscountPrice());
        updateRequest.setDiscountStartDate(createRequest.getDiscountStartDate());
        updateRequest.setDiscountEndDate(createRequest.getDiscountEndDate());
        updateRequest.setSupplierName(createRequest.getSupplierName());
        updateRequest.setIsPerishable(createRequest.getIsPerishable());
        updateRequest.setExpiryDate(createRequest.getExpiryDate());
        updateRequest.setIsActive(true);
        return updateRequest;
    }

    // Helper methods for parsing different data types

    private String getStringValue(String[] values, int index) {
        if (index >= values.length)
            return null;
        String value = values[index];
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        return value.trim();
    }

    private Integer parseInteger(String value) {
        if (value == null || value.trim().isEmpty())
            return null;
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private BigDecimal parseBigDecimal(String value) {
        if (value == null || value.trim().isEmpty())
            return null;
        try {
            return new BigDecimal(value.trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private Unit parseUnit(String value) {
        if (value == null || value.trim().isEmpty())
            return Unit.PIECES;
        try {
            return Unit.valueOf(value.trim().toUpperCase());
        } catch (IllegalArgumentException e) {
            return Unit.PIECES; // Default to PIECES if invalid
        }
    }

    private Money parseMoney(String amount, String currency) {
        if (amount == null || amount.trim().isEmpty())
            return null;
        try {
            BigDecimal amountValue = new BigDecimal(amount.trim());
            String currencyCode = (currency != null && !currency.trim().isEmpty()) ? currency.trim() : "USD";
            Currency currencyEnum = Currency.valueOf(currencyCode.toUpperCase());
            return new Money(amountValue, currencyEnum);
        } catch (NumberFormatException e) {
            return null;
        } catch (IllegalArgumentException e) {
            // Default to USD if currency is invalid
            try {
                BigDecimal amountValue = new BigDecimal(amount.trim());
                return new Money(amountValue, Currency.USD);
            } catch (NumberFormatException ex) {
                return null;
            }
        }
    }

    private LocalDateTime parseDateTime(String value) {
        if (value == null || value.trim().isEmpty())
            return null;
        try {
            // Try different date formats
            String[] formats = { "yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd", "dd/MM/yyyy", "MM/dd/yyyy" };
            for (String format : formats) {
                try {
                    if (format.contains("HH:mm:ss")) {
                        return LocalDateTime.parse(value.trim(), DateTimeFormatter.ofPattern(format));
                    } else {
                        return LocalDate.parse(value.trim(), DateTimeFormatter.ofPattern(format)).atStartOfDay();
                    }
                } catch (Exception ignored) {
                    // Continue to next format
                }
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    private LocalDate parseDate(String value) {
        if (value == null || value.trim().isEmpty())
            return null;
        try {
            String[] formats = { "yyyy-MM-dd", "dd/MM/yyyy", "MM/dd/yyyy" };
            for (String format : formats) {
                try {
                    return LocalDate.parse(value.trim(), DateTimeFormatter.ofPattern(format));
                } catch (Exception ignored) {
                    // Continue to next format
                }
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    private Boolean parseBoolean(String value) {
        if (value == null || value.trim().isEmpty())
            return false;
        String lowerValue = value.trim().toLowerCase();
        return "true".equals(lowerValue) || "yes".equals(lowerValue) || "1".equals(lowerValue);
    }
}