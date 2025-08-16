package com.sajjadkademm.retail.inventory.ExcelUpload;

/**
 * CSV Upload Module for Inventory Items
 * 
 * This module provides functionality to bulk create inventory items by
 * uploading CSV files.
 * 
 * Main Components:
 * - ExcelUploadService: Service for processing CSV files and creating inventory
 * items
 * - ExcelUploadController: REST controller for handling HTTP requests
 * - ExcelUploadUtils: Utility class for CSV validation and template generation
 * - DTOs: Request and response data transfer objects
 * 
 * Usage:
 * 1. Download CSV template using GET /api/v1/inventory/csv-upload/template
 * 2. Upload filled CSV file using POST /api/v1/inventory/csv-upload/upload
 * 3. Review results for successful and failed items
 */