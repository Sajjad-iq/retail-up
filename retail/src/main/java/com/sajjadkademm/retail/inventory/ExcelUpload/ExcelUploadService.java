package com.sajjadkademm.retail.inventory.ExcelUpload;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemRepository;
import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItemService;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Money;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.ExcelUpload.dto.ExcelUploadResponse;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.utils.dto.Currency;

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

    @Autowired
    public ExcelUploadService(InventoryItemService inventoryItemService,
            InventoryItemRepository inventoryItemRepository) {
        this.inventoryItemService = inventoryItemService;
        this.inventoryItemRepository = inventoryItemRepository;
    }

    /**
     * Process Excel file and create inventory items
     */
    @Transactional(rollbackFor = { BadRequestException.class })
    public ExcelUploadResponse processExcelFile(MultipartFile file, String inventoryId, User user) {
        try {
            List<CreateInventoryItemRequest> items = parseExcelFile(file, inventoryId); // List of items to be created

            if (items.isEmpty()) {
                throw new BadRequestException(
                        "No valid rows found in the CSV file. Please check the file format and ensure it contains data.");
            }

            List<InventoryItem> createdItems = new ArrayList<>(); // List of created items
            List<String> errors = new ArrayList<>(); // List of errors

            for (int i = 0; i < items.size(); i++) {
                try {
                    CreateInventoryItemRequest itemRequest = items.get(i);

                    // Check if the item already exists by SKU, barcode, or product code
                    Optional<InventoryItem> existingItem = Optional.empty();

                    if (itemRequest.getSku() != null && !itemRequest.getSku().trim().isEmpty()) {
                        existingItem = inventoryItemRepository.findBySkuAndInventoryId(itemRequest.getSku(),
                                inventoryId);
                    }

                    if (existingItem.isEmpty() && itemRequest.getBarcode() != null
                            && !itemRequest.getBarcode().trim().isEmpty()) {
                        existingItem = inventoryItemRepository.findByBarcodeAndInventoryId(itemRequest.getBarcode(),
                                inventoryId);
                    }

                    if (existingItem.isEmpty() && itemRequest.getProductCode() != null
                            && !itemRequest.getProductCode().trim().isEmpty()) {
                        existingItem = inventoryItemRepository
                                .findByProductCodeAndInventoryId(itemRequest.getProductCode(), inventoryId);
                    }

                    // If the item already exists, update it
                    if (existingItem.isPresent()) {
                        UpdateInventoryItemRequest updateRequest = new UpdateInventoryItemRequest();
                        updateRequest.setUserId(user.getId());
                        updateRequest.setName(itemRequest.getName());
                        updateRequest.setDescription(itemRequest.getDescription());
                        updateRequest.setSku(itemRequest.getSku());
                        updateRequest.setProductCode(itemRequest.getProductCode());
                        updateRequest.setBarcode(itemRequest.getBarcode());
                        updateRequest.setCategory(itemRequest.getCategory());
                        updateRequest.setBrand(itemRequest.getBrand());
                        updateRequest.setUnit(itemRequest.getUnit());
                        updateRequest.setWeight(itemRequest.getWeight());
                        updateRequest.setDimensions(itemRequest.getDimensions());
                        updateRequest.setColor(itemRequest.getColor());
                        updateRequest.setSize(itemRequest.getSize());
                        updateRequest.setCurrentStock(itemRequest.getCurrentStock());
                        updateRequest.setMinimumStock(itemRequest.getMinimumStock());
                        updateRequest.setMaximumStock(itemRequest.getMaximumStock());
                        updateRequest.setCostPrice(itemRequest.getCostPrice());
                        updateRequest.setSellingPrice(itemRequest.getSellingPrice());
                        updateRequest.setDiscountPrice(itemRequest.getDiscountPrice());
                        updateRequest.setDiscountStartDate(itemRequest.getDiscountStartDate());
                        updateRequest.setDiscountEndDate(itemRequest.getDiscountEndDate());
                        updateRequest.setSupplierName(itemRequest.getSupplierName());
                        updateRequest.setIsPerishable(itemRequest.getIsPerishable());
                        updateRequest.setExpiryDate(itemRequest.getExpiryDate());
                        updateRequest.setIsActive(true);

                        InventoryItem updated = inventoryItemService.updateInventoryItem(existingItem.get().getId(),
                                updateRequest);
                        createdItems.add(updated);
                    } else {
                        // If the item does not exist, create it
                        // Set the actual user ID before creating
                        itemRequest.setUserId(user.getId());
                        InventoryItem created = inventoryItemService.createInventoryItem(itemRequest);
                        createdItems.add(created);
                    }
                } catch (Exception e) {
                    // Log the error for debugging
                    String errorMessage = "Row " + (i + 2) + ": " + e.getMessage();
                    if (e.getCause() != null) {
                        errorMessage += " (Caused by: " + e.getCause().getMessage() + ")";
                    }
                    errors.add(errorMessage);
                }
            }

            return ExcelUploadResponse.builder()
                    .totalRows(items.size())
                    .successfulItems(createdItems.size())
                    .failedItems(errors.size())
                    .createdItems(createdItems)
                    .errors(errors)
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
            // Validate required fields first
            String name = getStringValue(values, 0);
            String unitStr = getStringValue(values, 7);
            String currentStockStr = getStringValue(values, 12);
            String sellingPriceAmountStr = getStringValue(values, 17);
            String sellingPriceCurrencyStr = getStringValue(values, 18);

            // Check required fields
            if (name == null || name.trim().isEmpty()) {
                throw new IllegalArgumentException("Name is required");
            }

            if (unitStr == null || unitStr.trim().isEmpty()) {
                throw new IllegalArgumentException("Unit is required");
            }

            if (currentStockStr == null || currentStockStr.trim().isEmpty()) {
                throw new IllegalArgumentException("Current stock is required");
            }

            if (sellingPriceAmountStr == null || sellingPriceAmountStr.trim().isEmpty()) {
                throw new IllegalArgumentException("Selling price amount is required");
            }

            if (sellingPriceCurrencyStr == null || sellingPriceCurrencyStr.trim().isEmpty()) {
                throw new IllegalArgumentException("Selling price currency is required");
            }

            CreateInventoryItemRequest item = new CreateInventoryItemRequest();
            item.setName(name);
            item.setDescription(getStringValue(values, 1));
            item.setSku(getStringValue(values, 2));
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

            // Handle expiry date - if empty string, set to null
            String expiryDateStr = getStringValue(values, 24);
            if (expiryDateStr != null && expiryDateStr.trim().isEmpty()) {
                item.setExpiryDate(null);
            } else {
                item.setExpiryDate(parseDate(expiryDateStr));
            }

            item.setInventoryId(inventoryId);
            item.setUserId("temp-user-id"); // This id will be overridden by the user id in the controller

            return item;
        } catch (Exception e) {
            // Return null for invalid rows, they will be counted as errors
            return null;
        }
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