# CSV Upload Module for Inventory Items

This module provides functionality to bulk create inventory items by uploading CSV files.

## Overview

The CSV Upload module allows users to create multiple inventory items at once by uploading a CSV file containing item details. The module processes the CSV file and creates inventory items using the existing `InventoryItemService`.

## Features

- **Bulk Upload**: Create multiple inventory items from a single CSV file
- **Error Handling**: Detailed error reporting for failed items
- **Template Download**: Get a CSV template with the correct column structure
- **Validation**: Basic validation of CSV format and data types
- **Transaction Safety**: All operations are wrapped in transactions

## API Endpoints

### 1. Upload CSV File
```
POST /api/v1/inventory/csv-upload/upload
Content-Type: multipart/form-data

Parameters:
- file: CSV file to upload
- inventoryId: ID of the inventory where items will be created
- userId: ID of the user performing the upload
```

### 2. Download CSV Template
```
GET /api/v1/inventory/csv-upload/template
```

Returns a CSV template file with headers and example data.

## CSV Format

The CSV file should have the following columns (in order):

| Column | Field | Required | Description |
|--------|-------|----------|-------------|
| A | Name | Yes | Product name |
| B | Description | No | Product description |
| C | SKU | No | Stock keeping unit |
| D | Product Code | No | Internal product code |
| E | Barcode | No | Product barcode |
| F | Category | No | Product category |
| G | Brand | No | Product brand |
| H | Unit | Yes | Measurement unit (PIECES, KG, LITERS, etc.) |
| I | Weight | No | Product weight |
| J | Dimensions | No | Product dimensions |
| K | Color | No | Product color |
| L | Size | No | Product size |
| M | Current Stock | Yes | Initial stock quantity |
| N | Minimum Stock | Yes | Minimum stock level |
| O | Maximum Stock | No | Maximum stock capacity |
| P | Cost Price Amount | No | Cost price amount |
| Q | Cost Price Currency | No | Cost price currency (USD, EUR, etc.) |
| R | Selling Price Amount | Yes | Selling price amount |
| S | Selling Price Currency | Yes | Selling price currency |
| T | Discount Price | No | Discounted price |
| U | Discount Start Date | No | Discount start date (YYYY-MM-DD) |
| V | Discount End Date | No | Discount end date (YYYY-MM-DD) |
| W | Supplier Name | No | Supplier name |
| X | Is Perishable | No | Whether item is perishable (true/false) |
| Y | Expiry Date | No | Expiry date (YYYY-MM-DD) |

## Usage Example

1. **Download Template**: First, download the CSV template using the `/template` endpoint
2. **Fill Data**: Fill in the template with your inventory item data
3. **Upload**: Upload the filled CSV file using the `/upload` endpoint
4. **Review Results**: Check the response for successful and failed items

## Response Format

The upload endpoint returns a response with:

```json
{
  "totalRows": 10,
  "successfulItems": 8,
  "failedItems": 2,
  "createdItems": [...],
  "errors": [
    "Row 3: Invalid unit value",
    "Row 7: Selling price is required"
  ]
}
```

## Error Handling

- **File Validation**: Checks if the uploaded file is a valid CSV
- **Data Validation**: Validates each row against business rules
- **Partial Success**: Continues processing even if some rows fail
- **Detailed Error Messages**: Provides specific error information for each failed row

## Dependencies

- `InventoryItemService`: For creating inventory items
- `UserService`: For user validation
- Spring Framework: For web functionality and transactions

## Notes

- The module currently supports CSV files (not Excel files)
- All dates should be in YYYY-MM-DD format
- Currency codes should be valid ISO currency codes (USD, EUR, etc.)
- Unit values should match the Unit enum values (PIECES, KG, LITERS, etc.)
- The module sets default values for optional fields when not provided 