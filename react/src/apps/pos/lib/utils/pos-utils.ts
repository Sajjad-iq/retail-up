/**
 * Utility functions for POS system
 */

/**
 * Format a number as a price string
 * @param price - The price to format
 * @returns Formatted price string (e.g., "$12.34")
 */
export function formatPrice(price: number): string {
    return `$${price.toFixed(2)}`;
}

/**
 * Format a date as a time string
 * @param date - The date to format
 * @returns Formatted time string (e.g., "2:30 PM")
 */
export function formatTime(date: Date): string {
    return date.toLocaleTimeString([], {
        hour: '2-digit',
        minute: '2-digit',
    });
}

/**
 * Format a date as a readable string
 * @param date - The date to format
 * @returns Formatted date string
 */
export function formatDate(date: Date): string {
    return date.toLocaleDateString();
}

/**
 * Generate a unique transaction ID
 * @returns Unique transaction ID
 */
export function generateTransactionId(): string {
    return `TXN-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Calculate tax amount based on subtotal
 * @param subtotal - The subtotal amount
 * @param taxRate - The tax rate (default: 0.08 for 8%)
 * @returns Tax amount
 */
export function calculateTax(subtotal: number, taxRate: number = 0.08): number {
    return subtotal * taxRate;
}

/**
 * Calculate total with tax
 * @param subtotal - The subtotal amount
 * @param taxRate - The tax rate (default: 0.08 for 8%)
 * @returns Total amount including tax
 */
export function calculateTotal(subtotal: number, taxRate: number = 0.08): number {
    return subtotal + calculateTax(subtotal, taxRate);
}

/**
 * Validate if a product has sufficient stock
 * @param stock - Current stock level
 * @param requestedQuantity - Requested quantity
 * @returns True if stock is sufficient
 */
export function hasAvailableStock(stock: number, requestedQuantity: number): boolean {
    return stock >= requestedQuantity;
}

/**
 * Check if a product is low stock
 * @param stock - Current stock level
 * @param threshold - Low stock threshold (default: 5)
 * @returns True if stock is low
 */
export function isLowStock(stock: number, threshold: number = 5): boolean {
    return stock > 0 && stock <= threshold;
}

/**
 * Check if a product is out of stock
 * @param stock - Current stock level
 * @returns True if out of stock
 */
export function isOutOfStock(stock: number): boolean {
    return stock === 0;
} 