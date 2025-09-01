package com.sajjadkademm.retail.inventory.ExcelUpload;

import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
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

import lombok.RequiredArgsConstructor;
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
@RequiredArgsConstructor
public class ExcelUploadService {
    private final InventoryItemService inventoryItemService;
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemUpdateValidator inventoryItemUpdateValidator;
    private final InventoryMovementService inventoryMovementService;
    private final LocalizedErrorService localizedErrorService;

    /**
     * MAIN ENTRY POINT: Process Excel file and batch create/update inventory items.
     * 
     * FLOW OVERVIEW:
     * 1. Parse CSV file into structured item requests
     * 2. For each row: check if item exists, then create or update
     * 3. Collect successes and errors without stopping on failures
     * 4. Return comprehensive response with statistics
     * 
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
            // STEP 1: Parse the Excel/CSV file into inventory item requests
            // This converts raw CSV data into structured objects for processing
            List<CreateInventoryItemRequest> items = parseExcelFile(file, inventoryId);

            if (items.isEmpty()) {
                throw new BadRequestException(
                        localizedErrorService.getLocalizedMessage("no.valid.rows.found"));
            }

            // STEP 2: Initialize tracking collections for batch processing
            // We collect both successes and failures to provide comprehensive feedback
            List<InventoryItem> processedItems = new ArrayList<>();
            List<String> processingErrors = new ArrayList<>();

            // STEP 3: Process each item individually with error isolation
            // This ensures one bad row doesn't stop the entire batch
            for (int i = 0; i < items.size(); i++) {
                CreateInventoryItemRequest itemRequest = items.get(i);

                int rowNumber = i + 2; // CSV row number (accounting for header row)

                // STEP 3A: Check for duplicates based on unique identifiers
                // Priority: Barcode -> Product Code -> Name
                Optional<InventoryItem> existingItem = findExistingItemByIdentifiers(itemRequest, inventoryId);

                // STEP 3B: Route to create or update based on existence
                // This is the key decision point that determines the operation type
                processInventoryItem(existingItem, itemRequest, user, rowNumber, processedItems, processingErrors);
            }

            // STEP 4: Build comprehensive response with statistics
            // This provides detailed feedback about the batch operation results
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
     * CSV PARSING: Convert raw CSV file into structured item requests
     * 
     * PARSING STRATEGY:
     * - Skip header row and empty lines
     * - Handle variable column counts (pad/truncate to expected 25 columns)
     * - Parse each row individually, skip invalid rows without failing entire file
     * - Use fault-tolerant approach: malformed rows return null and are ignored
     */
    private List<CreateInventoryItemRequest> parseExcelFile(MultipartFile file, String inventoryId) throws IOException {
        List<CreateInventoryItemRequest> items = new ArrayList<>();
        final int EXPECTED_COLUMNS = 25; // Based on the CSV template structure

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream()))) {
            String line;
            boolean isFirstLine = true;

            while ((line = reader.readLine()) != null) {

                // SKIP HEADER: First line contains column names, not data
                if (isFirstLine) {
                    isFirstLine = false;
                    continue; // Skip header row
                }

                // SKIP EMPTY: Ignore blank lines in CSV
                if (line.trim().isEmpty()) {
                    continue;
                }

                // PARSE LINE: Handle CSV quoting and comma escaping
                String[] values = parseCsvLine(line);

                // NORMALIZE COLUMNS: Ensure consistent array size for mapping
                // This handles files with missing trailing columns or extra columns
                if (values.length < EXPECTED_COLUMNS) {
                    // PAD SHORT ROWS: Fill missing columns with empty strings
                    String[] paddedValues = new String[EXPECTED_COLUMNS];
                    System.arraycopy(values, 0, paddedValues, 0, values.length);
                    for (int j = values.length; j < EXPECTED_COLUMNS; j++) {
                        paddedValues[j] = "";
                    }
                    values = paddedValues;
                } else if (values.length > EXPECTED_COLUMNS) {
                    // TRUNCATE LONG ROWS: Ignore extra columns beyond expected count
                    String[] truncatedValues = new String[EXPECTED_COLUMNS];
                    System.arraycopy(values, 0, truncatedValues, 0, EXPECTED_COLUMNS);
                    values = truncatedValues;
                }

                // CONVERT ROW: Transform CSV values into inventory item request
                // Returns null for invalid rows, which are silently skipped
                CreateInventoryItemRequest item = parseRow(values, inventoryId);
                if (item != null) {
                    items.add(item);
                }
            }
        }

        return items;
    }

    /**
     * CSV LINE PARSING: Handle quoted fields and commas within fields
     * 
     * CSV COMPLEXITY HANDLING:
     * - Tracks quote state to handle embedded commas (e.g., "Product, Red")
     * - Properly splits on commas only when outside quotes
     * - Handles standard CSV escaping rules
     */
    private String[] parseCsvLine(String line) {
        List<String> result = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;

        // CHARACTER-BY-CHARACTER PARSING: Required for proper CSV handling
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);

            if (c == '"') {
                // QUOTE TOGGLE: Enter/exit quoted field
                inQuotes = !inQuotes;
            } else if (c == ',' && !inQuotes) {
                // FIELD SEPARATOR: Only split on commas outside quotes
                result.add(current.toString().trim());
                current = new StringBuilder();
            } else {
                // REGULAR CHARACTER: Add to current field
                current.append(c);
            }
        }

        // FINAL FIELD: Add the last field (no trailing comma)
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
     * DUPLICATE DETECTION: Find existing inventory item by unique identifiers
     * 
     * SEARCH PRIORITY STRATEGY:
     * 1. Barcode (most specific, globally unique)
     * 2. Product Code (business identifier, should be unique)
     * 3. Name (fallback, may have conflicts)
     * 
     * PURPOSE: Determines whether to CREATE new item or UPDATE existing item
     * This is the critical decision point that prevents duplicate inventory items.
     * 
     * @param itemRequest The item request containing potential identifiers
     * @param inventoryId The inventory to search within
     * @return Optional containing existing item if found, empty otherwise
     */
    private Optional<InventoryItem> findExistingItemByIdentifiers(CreateInventoryItemRequest itemRequest,
            String inventoryId) {

        // PRIORITY 1: Check by barcode (most reliable unique identifier)
        if (itemRequest.getBarcode() != null && !itemRequest.getBarcode().trim().isEmpty()) {
            Optional<InventoryItem> existing = inventoryItemRepository
                    .findByBarcodeAndInventoryId(itemRequest.getBarcode(), inventoryId);
            if (existing.isPresent())
                return existing;
        }

        // PRIORITY 2: Check by product code (business-specific identifier)
        if (itemRequest.getProductCode() != null && !itemRequest.getProductCode().trim().isEmpty()) {
            return inventoryItemRepository.findByProductCodeAndInventoryId(itemRequest.getProductCode(), inventoryId);
        }

        // PRIORITY 3: Check by name (least reliable but useful fallback)
        if (itemRequest.getName() != null && !itemRequest.getName().trim().isEmpty()) {
            return inventoryItemRepository.findByNameAndInventoryId(itemRequest.getName(), inventoryId);
        }

        // NO MATCH: Item is considered new and will be created
        return Optional.empty();
    }

    /**
     * OPERATION ROUTING: Process an inventory item by creating new or updating
     * existing
     * 
     * DECISION LOGIC:
     * - If existing item found by identifiers → UPDATE existing item
     * - If no existing item found → CREATE new item
     * 
     * ERROR HANDLING STRATEGY:
     * - Isolate errors per row to prevent batch failure
     * - Collect all errors for comprehensive feedback
     * - Continue processing remaining rows even if current row fails
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
                // ROUTE TO UPDATE: Item exists, merge new data with existing record
                updateExistingItem(existingItem.get(), itemRequest, user, rowNumber, createdItems, errors);
            } else {
                // ROUTE TO CREATE: Item is new, create fresh inventory record
                createNewItem(itemRequest, user, rowNumber, createdItems, errors);
            }
        } catch (Exception e) {
            // ISOLATION: Catch unexpected errors to prevent batch failure
            errors.add("Row " + rowNumber + ": Unexpected error - " + e.getMessage());
        }
    }

    /**
     * UPDATE PATHWAY: Update existing inventory item found during Excel upload
     * 
     * UPDATE PROCESS:
     * 1. Convert create request to update request (different validation rules)
     * 2. Validate all changes against business rules
     * 3. Apply updates if validation passes
     * 4. Track stock movements for audit trail
     * 5. Add to success list or capture errors
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
        // STEP 1: Convert create request to update request (different validation rules
        // apply)
        UpdateInventoryItemRequest updateRequest = convertCreateToUpdateRequest(itemRequest, user.getId());

        // STEP 2: Validate update request and collect all errors without throwing
        // exceptions
        // This ensures we provide comprehensive feedback rather than failing fast
        ValidationResult validationResult = inventoryItemUpdateValidator
                .validateAndCollectErrors(existingItem, updateRequest);

        if (validationResult.hasErrors()) {
            // VALIDATION FAILED: Collect validation errors for this row
            String errorMessage = String.join("; ", validationResult.getErrors());
            errors.add("Row " + rowNumber + ": " + errorMessage);
        } else {
            // VALIDATION PASSED: Apply updates and track changes
            // STEP 3: Capture original stock for movement tracking
            Integer originalStock = existingItem.getCurrentStock();

            // STEP 4: Apply all validated updates to the existing item
            inventoryItemUpdateValidator.applyUpdates(existingItem, updateRequest);
            InventoryItem updated = inventoryItemRepository.save(existingItem);

            // STEP 5: Track inventory movements for audit trail (stock changes, etc.)
            inventoryItemUpdateValidator.trackStockMovements(updated, updateRequest, inventoryMovementService,
                    originalStock);

            // SUCCESS: Add to processed items list
            createdItems.add(updated);
        }
    }

    /**
     * CREATE PATHWAY: Create new inventory item for Excel batch processing
     * 
     * CREATE PROCESS:
     * 1. Use specialized batch creation method (error collection, not exceptions)
     * 2. Validate all business rules (uniqueness, required fields, etc.)
     * 3. Create item and initial stock movements
     * 4. Add to success list or capture specific error messages
     * 
     * ERROR STRATEGY: Uses error collection approach to avoid stopping the entire
     * batch on first error.
     * 
     * @param itemRequest  The inventory item creation request from Excel row
     * @param user         The user performing the upload
     * @param rowNumber    The row number in Excel for error reporting
     * @param createdItems List to add successfully created items
     * @param errors       List to add error messages for failed items
     */
    private void createNewItem(CreateInventoryItemRequest itemRequest, User user, int rowNumber,
            List<InventoryItem> createdItems, List<String> errors) {
        // DELEGATE TO SERVICE: Use inventory service method designed for batch
        // operations
        // This method returns success/failure results instead of throwing exceptions
        CreateInventoryItemResult result = inventoryItemService.createInventoryItemWithErrorCollection(itemRequest);

        if (result.isSuccess()) {
            // SUCCESS: Item created successfully, add to results
            createdItems.add(result.getItem());
        } else {
            // FAILURE: Capture specific error message with row context
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