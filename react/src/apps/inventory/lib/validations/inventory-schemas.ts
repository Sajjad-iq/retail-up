import { z } from 'zod';

/**
 * Supplier validation schema
 */
export const supplierSchema = z.object({
    id: z.string().min(1, 'Supplier ID is required'),
    name: z.string().min(1, 'Supplier name is required'),
    contactPerson: z.string().optional(),
    email: z.string().email('Invalid email format').optional().or(z.literal('')),
    phone: z.string().min(10, 'Phone number must be at least 10 digits').optional().or(z.literal('')),
    address: z.string().optional(),
    paymentTerms: z.string().optional(),
});

/**
 * Category validation schema
 */
export const categorySchema = z.object({
    id: z.string().min(1, 'Category ID is required'),
    name: z.string().min(1, 'Category name is required'),
    description: z.string().optional(),
    parentId: z.string().optional(),
    color: z.string().optional(),
});

/**
 * Product status validation schema
 */
export const productStatusSchema = z.enum(['active', 'inactive', 'discontinued'], {
    errorMap: () => ({ message: 'Invalid product status' }),
});

/**
 * Inventory item validation schema
 */
export const inventoryItemSchema = z.object({
    id: z.string().min(1, 'Item ID is required'),
    name: z.string().min(1, 'Product name is required'),
    description: z.string().optional(),
    category: z.string().min(1, 'Category is required'),
    sku: z.string().min(3, 'SKU must be at least 3 characters'),
    barcode: z.string().optional(),
    costPrice: z.number().min(0, 'Cost price cannot be negative'),
    sellingPrice: z.number().min(0, 'Selling price cannot be negative'),
    currentStock: z.number().int().min(0, 'Current stock cannot be negative'),
    minimumStock: z.number().int().min(0, 'Minimum stock cannot be negative'),
    maximumStock: z.number().int().min(1, 'Maximum stock must be at least 1'),
    reorderQuantity: z.number().int().min(1, 'Reorder quantity must be at least 1'),
    location: z.string().optional(),
    supplier: supplierSchema.optional(),
    image: z.string().url('Invalid image URL').optional().or(z.literal('')),
    status: productStatusSchema,
    createdAt: z.date(),
    updatedAt: z.date(),
}).refine((data) => data.sellingPrice >= data.costPrice, {
    message: 'Selling price should be greater than or equal to cost price',
    path: ['sellingPrice'],
}).refine((data) => data.maximumStock >= data.minimumStock, {
    message: 'Maximum stock must be greater than or equal to minimum stock',
    path: ['maximumStock'],
});

/**
 * Inventory item form data validation schema
 */
export const inventoryItemFormSchema = z.object({
    name: z.string().min(1, 'Product name is required').max(100, 'Product name too long'),
    description: z.string().max(500, 'Description too long').optional(),
    categoryId: z.string().min(1, 'Category is required'),
    sku: z.string().min(3, 'SKU must be at least 3 characters').max(50, 'SKU too long'),
    barcode: z.string().max(20, 'Barcode too long').optional().or(z.literal('')),
    costPrice: z.number().min(0, 'Cost price cannot be negative'),
    sellingPrice: z.number().min(0, 'Selling price cannot be negative'),
    currentStock: z.number().int().min(0, 'Current stock cannot be negative'),
    minimumStock: z.number().int().min(0, 'Minimum stock cannot be negative'),
    maximumStock: z.number().int().min(1, 'Maximum stock must be at least 1'),
    reorderQuantity: z.number().int().min(1, 'Reorder quantity must be at least 1'),
    location: z.string().max(100, 'Location too long').optional(),
    supplierId: z.string().optional(),
    status: productStatusSchema,
}).refine((data) => data.sellingPrice >= data.costPrice, {
    message: 'Selling price should be greater than or equal to cost price',
    path: ['sellingPrice'],
}).refine((data) => data.maximumStock >= data.minimumStock, {
    message: 'Maximum stock must be greater than or equal to minimum stock',
    path: ['maximumStock'],
});

/**
 * Movement type validation schema
 */
export const movementTypeSchema = z.enum(['in', 'out', 'adjustment'], {
    errorMap: () => ({ message: 'Invalid movement type' }),
});

/**
 * Movement reason validation schema
 */
export const movementReasonSchema = z.enum([
    'purchase', 'sale', 'return', 'damage', 'expired', 'transfer', 'adjustment', 'initial'
], {
    errorMap: () => ({ message: 'Invalid movement reason' }),
});

/**
 * Stock movement validation schema
 */
export const stockMovementSchema = z.object({
    id: z.string().min(1, 'Movement ID is required'),
    inventoryItemId: z.string().min(1, 'Inventory item ID is required'),
    type: movementTypeSchema,
    reason: movementReasonSchema,
    quantity: z.number().int().refine((val) => val !== 0, {
        message: 'Quantity cannot be zero',
    }),
    previousStock: z.number().int().min(0, 'Previous stock cannot be negative'),
    newStock: z.number().int().min(0, 'New stock cannot be negative'),
    reference: z.string().optional(),
    notes: z.string().max(500, 'Notes too long').optional(),
    userId: z.string().optional(),
    timestamp: z.date(),
});

/**
 * Stock adjustment form validation schema
 */
export const stockAdjustmentFormSchema = z.object({
    itemId: z.string().min(1, 'Item is required'),
    quantity: z.number().int().refine((val) => val !== 0, {
        message: 'Adjustment quantity cannot be zero',
    }),
    reason: movementReasonSchema,
    reference: z.string().max(50, 'Reference too long').optional(),
    notes: z.string().max(500, 'Notes too long').optional(),
});

/**
 * Low stock alert validation schema
 */
export const lowStockAlertSchema = z.object({
    id: z.string().min(1, 'Alert ID is required'),
    inventoryItem: inventoryItemSchema,
    severity: z.enum(['warning', 'critical'], {
        errorMap: () => ({ message: 'Invalid alert severity' }),
    }),
    message: z.string().min(1, 'Alert message is required'),
    createdAt: z.date(),
    acknowledged: z.boolean(),
});

/**
 * Search filters validation schema
 */
export const inventoryFiltersSchema = z.object({
    query: z.string().max(100, 'Search query too long').optional(),
    category: z.string().optional(),
    status: productStatusSchema.optional(),
    supplier: z.string().optional(),
    stockLevel: z.enum(['all', 'low', 'out', 'normal'], {
        errorMap: () => ({ message: 'Invalid stock level filter' }),
    }).optional(),
    sortBy: z.enum(['name', 'stock', 'price', 'updatedAt'], {
        errorMap: () => ({ message: 'Invalid sort field' }),
    }).optional(),
    sortOrder: z.enum(['asc', 'desc'], {
        errorMap: () => ({ message: 'Invalid sort order' }),
    }).optional(),
});

/**
 * SKU validation schema
 */
export const skuValidationSchema = z.object({
    sku: z.string().min(3, 'SKU must be at least 3 characters').regex(
        /^[A-Z0-9-]+$/i,
        'SKU can only contain letters, numbers, and hyphens'
    ),
});

/**
 * Barcode validation schema
 */
export const barcodeValidationSchema = z.object({
    barcode: z.string().regex(
        /^\d{8,13}$/,
        'Barcode must be 8-13 digits'
    ),
});

/**
 * Bulk update validation schema
 */
export const bulkUpdateSchema = z.object({
    itemIds: z.array(z.string().min(1, 'Item ID required')).min(1, 'At least one item must be selected'),
    updateData: z.object({
        category: z.string().optional(),
        status: productStatusSchema.optional(),
        supplier: z.string().optional(),
        location: z.string().optional(),
    }).refine((data) => Object.keys(data).length > 0, {
        message: 'At least one field must be updated',
    }),
});

/**
 * Import validation schema
 */
export const importDataSchema = z.object({
    items: z.array(inventoryItemFormSchema).min(1, 'At least one item is required'),
    validateOnly: z.boolean().optional(),
});

// Type exports
export type SupplierInput = z.infer<typeof supplierSchema>;
export type CategoryInput = z.infer<typeof categorySchema>;
export type InventoryItemInput = z.infer<typeof inventoryItemSchema>;
export type InventoryItemFormInput = z.infer<typeof inventoryItemFormSchema>;
export type StockMovementInput = z.infer<typeof stockMovementSchema>;
export type StockAdjustmentFormInput = z.infer<typeof stockAdjustmentFormSchema>;
export type LowStockAlertInput = z.infer<typeof lowStockAlertSchema>;
export type InventoryFiltersInput = z.infer<typeof inventoryFiltersSchema>;
export type SKUValidationInput = z.infer<typeof skuValidationSchema>;
export type BarcodeValidationInput = z.infer<typeof barcodeValidationSchema>;
export type BulkUpdateInput = z.infer<typeof bulkUpdateSchema>;
export type ImportDataInput = z.infer<typeof importDataSchema>; 