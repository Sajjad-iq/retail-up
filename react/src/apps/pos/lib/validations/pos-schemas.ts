import { z } from 'zod';

/**
 * Product validation schema
 */
export const productSchema = z.object({
    id: z.string().min(1, 'Product ID is required'),
    name: z.string().min(1, 'Product name is required'),
    price: z.number().positive('Price must be positive'),
    category: z.string().min(1, 'Category is required'),
    image: z.string().url().optional(),
    barcode: z.string().optional(),
    stock: z.number().int().min(0, 'Stock cannot be negative'),
});

/**
 * Cart item validation schema
 */
export const cartItemSchema = z.object({
    product: productSchema,
    quantity: z.number().int().positive('Quantity must be positive'),
});

/**
 * Customer validation schema
 */
export const customerSchema = z.object({
    id: z.string().min(1, 'Customer ID is required'),
    name: z.string().min(1, 'Customer name is required'),
    email: z.string().email('Invalid email format').optional(),
    phone: z.string().min(10, 'Phone number must be at least 10 digits').optional(),
});

/**
 * Payment method validation schema
 */
export const paymentMethodSchema = z.enum(['cash', 'card', 'mobile'], {
    errorMap: () => ({ message: 'Invalid payment method' }),
});

/**
 * Payment form validation schema
 */
export const paymentFormSchema = z.object({
    paymentMethod: paymentMethodSchema,
    amountReceived: z.number().positive('Amount must be positive').optional(),
}).refine((data) => {
    // If payment method is cash, amount received is required
    if (data.paymentMethod === 'cash') {
        return data.amountReceived !== undefined;
    }
    return true;
}, {
    message: 'Amount received is required for cash payments',
    path: ['amountReceived'],
});

/**
 * Transaction validation schema
 */
export const transactionSchema = z.object({
    id: z.string().min(1, 'Transaction ID is required'),
    items: z.array(cartItemSchema).min(1, 'At least one item is required'),
    customer: customerSchema.nullable().optional(),
    subtotal: z.number().min(0, 'Subtotal cannot be negative'),
    tax: z.number().min(0, 'Tax cannot be negative'),
    total: z.number().positive('Total must be positive'),
    paymentMethod: paymentMethodSchema,
    timestamp: z.date(),
});

/**
 * Search query validation schema
 */
export const searchQuerySchema = z.object({
    query: z.string().max(100, 'Search query too long'),
    category: z.string().optional(),
});

/**
 * Quantity update validation schema
 */
export const quantityUpdateSchema = z.object({
    productId: z.string().min(1, 'Product ID is required'),
    quantity: z.number().int().min(1, 'Quantity must be at least 1'),
});

/**
 * Cash payment validation schema
 */
export const cashPaymentSchema = z.object({
    amountReceived: z.number().positive('Amount received must be positive'),
    total: z.number().positive('Total must be positive'),
}).refine((data) => data.amountReceived >= data.total, {
    message: 'Amount received must be greater than or equal to total',
    path: ['amountReceived'],
});

// Type exports
export type ProductInput = z.infer<typeof productSchema>;
export type CartItemInput = z.infer<typeof cartItemSchema>;
export type CustomerInput = z.infer<typeof customerSchema>;
export type PaymentFormInput = z.infer<typeof paymentFormSchema>;
export type TransactionInput = z.infer<typeof transactionSchema>;
export type SearchQueryInput = z.infer<typeof searchQuerySchema>;
export type QuantityUpdateInput = z.infer<typeof quantityUpdateSchema>;
export type CashPaymentInput = z.infer<typeof cashPaymentSchema>; 