/**
 * Utility functions for Inventory Management system
 */

import type { InventoryItem, LowStockAlert, StockMovement, CategoryBreakdown } from '../../types/inventory';

/**
 * Format a number as a price string
 * @param price - The price to format
 * @returns Formatted price string (e.g., "$12.34")
 */
export function formatPrice(price: number): string {
    return `$${price.toFixed(2)}`;
}

/**
 * Format a number as a currency string
 * @param amount - The amount to format
 * @param currency - The currency symbol (default: "$")
 * @returns Formatted currency string
 */
export function formatCurrency(amount: number, currency: string = '$'): string {
    return `${currency}${amount.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
}

/**
 * Format a date as a readable string
 * @param date - The date to format (can be Date object or string)
 * @returns Formatted date string
 */
export function formatDate(date: Date | string): string {
    const dateObj = typeof date === 'string' ? new Date(date) : date;

    // Check if the date is valid
    if (isNaN(dateObj.getTime())) {
        console.warn('Invalid date provided to formatDate:', date);
        return 'Invalid Date';
    }

    return dateObj.toLocaleDateString();
}

/**
 * Format a date as a time string
 * @param date - The date to format (can be Date object or string)
 * @returns Formatted time string (e.g., "2:30 PM")
 */
export function formatTime(date: Date | string): string {
    const dateObj = typeof date === 'string' ? new Date(date) : date;

    // Check if the date is valid
    if (isNaN(dateObj.getTime())) {
        console.warn('Invalid date provided to formatTime:', date);
        return 'Invalid Time';
    }

    return dateObj.toLocaleTimeString([], {
        hour: '2-digit',
        minute: '2-digit',
    });
}

/**
 * Format a date as a full datetime string
 * @param date - The date to format (can be Date object or string)
 * @returns Formatted datetime string
 */
export function formatDateTime(date: Date | string): string {
    return `${formatDate(date)} ${formatTime(date)}`;
}

/**
 * Generate a unique item ID
 * @returns Unique item ID
 */
export function generateItemId(): string {
    return `ITEM-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Generate a unique SKU code
 * @param category - Product category (optional)
 * @returns Generated SKU
 */
export function generateSKU(category?: string): string {
    const prefix = category ? category.substring(0, 3).toUpperCase() : 'GEN';
    const timestamp = Date.now().toString().slice(-6);
    const random = Math.random().toString(36).substr(2, 3).toUpperCase();
    return `${prefix}-${timestamp}-${random}`;
}

/**
 * Generate a unique movement ID
 * @returns Unique movement ID
 */
export function generateMovementId(): string {
    return `MOV-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Check if an inventory item is low stock
 * @param item - The inventory item to check
 * @returns True if stock is low
 */
export function isLowStock(item: InventoryItem): boolean {
    return item.currentStock > 0 && item.currentStock <= item.minimumStock;
}

/**
 * Check if an inventory item is out of stock
 * @param item - The inventory item to check
 * @returns True if out of stock
 */
export function isOutOfStock(item: InventoryItem): boolean {
    return item.currentStock === 0;
}

/**
 * Check if an inventory item is overstocked
 * @param item - The inventory item to check
 * @returns True if overstocked
 */
export function isOverstocked(item: InventoryItem): boolean {
    return item.currentStock > item.maximumStock;
}

/**
 * Get stock status for an inventory item
 * @param item - The inventory item to check
 * @returns Stock status
 */
export function getStockStatus(item: InventoryItem): 'out' | 'low' | 'normal' | 'overstocked' {
    if (isOutOfStock(item)) return 'out';
    if (isLowStock(item)) return 'low';
    if (isOverstocked(item)) return 'overstocked';
    return 'normal';
}

/**
 * Calculate profit margin for an inventory item
 * @param costPrice - Cost price of the item
 * @param sellingPrice - Selling price of the item
 * @returns Profit margin as a percentage
 */
export function calculateProfitMargin(costPrice: number, sellingPrice: number): number {
    if (costPrice === 0) return 0;
    return ((sellingPrice - costPrice) / costPrice) * 100;
}

/**
 * Calculate total inventory value
 * @param items - Array of inventory items
 * @returns Total value at cost price
 */
export function calculateInventoryValue(items: InventoryItem[]): number {
    return items.reduce((total, item) => total + (item.costPrice * item.currentStock), 0);
}

/**
 * Calculate total retail value
 * @param items - Array of inventory items
 * @returns Total value at selling price
 */
export function calculateRetailValue(items: InventoryItem[]): number {
    return items.reduce((total, item) => total + (item.sellingPrice * item.currentStock), 0);
}

/**
 * Get low stock alerts for inventory items
 * @param items - Array of inventory items
 * @returns Array of low stock alerts
 */
export function getLowStockAlerts(items: InventoryItem[]): LowStockAlert[] {
    return items
        .filter(item => isLowStock(item) || isOutOfStock(item))
        .map(item => ({
            id: `ALERT-${item.id}-${Date.now()}`,
            inventoryItem: item,
            severity: isOutOfStock(item) ? 'critical' as const : 'warning' as const,
            message: isOutOfStock(item)
                ? `${item.name} is out of stock`
                : `${item.name} is running low (${item.currentStock} remaining)`,
            createdAt: new Date(),
            acknowledged: false,
        }));
}

/**
 * Filter inventory items by search query
 * @param items - Array of inventory items
 * @param query - Search query
 * @returns Filtered items
 */
export function filterItemsByQuery(items: InventoryItem[], query: string): InventoryItem[] {
    if (!query.trim()) return items;

    const searchTerm = query.toLowerCase();
    return items.filter(item =>
        item.name.toLowerCase().includes(searchTerm) ||
        item.category.toLowerCase().includes(searchTerm) ||
        item.sku.toLowerCase().includes(searchTerm) ||
        item.barcode?.toLowerCase().includes(searchTerm) ||
        item.description?.toLowerCase().includes(searchTerm)
    );
}

/**
 * Sort inventory items by specified field
 * @param items - Array of inventory items
 * @param sortBy - Field to sort by
 * @param order - Sort order (asc or desc)
 * @returns Sorted items
 */
export function sortItems(
    items: InventoryItem[],
    sortBy: 'name' | 'stock' | 'price' | 'updatedAt',
    order: 'asc' | 'desc' = 'asc'
): InventoryItem[] {
    return [...items].sort((a, b) => {
        let aValue: any;
        let bValue: any;

        switch (sortBy) {
            case 'name':
                aValue = a.name.toLowerCase();
                bValue = b.name.toLowerCase();
                break;
            case 'stock':
                aValue = a.currentStock;
                bValue = b.currentStock;
                break;
            case 'price':
                aValue = a.sellingPrice;
                bValue = b.sellingPrice;
                break;
            case 'updatedAt':
                aValue = new Date(a.updatedAt).getTime();
                bValue = new Date(b.updatedAt).getTime();
                break;
            default:
                aValue = a.name;
                bValue = b.name;
        }

        if (aValue < bValue) return order === 'asc' ? -1 : 1;
        if (aValue > bValue) return order === 'asc' ? 1 : -1;
        return 0;
    });
}

/**
 * Group inventory items by category
 * @param items - Array of inventory items
 * @returns Items grouped by category
 */
export function groupItemsByCategory(items: InventoryItem[]): Record<string, InventoryItem[]> {
    return items.reduce((groups, item) => {
        const category = item.category || 'Uncategorized';
        if (!groups[category]) {
            groups[category] = [];
        }
        groups[category].push(item);
        return groups;
    }, {} as Record<string, InventoryItem[]>);
}

/**
 * Get category breakdown for reporting
 * @param items - Array of inventory items
 * @returns Category breakdown data
 */
export function getCategoryBreakdown(items: InventoryItem[]): CategoryBreakdown[] {
    const groups = groupItemsByCategory(items);

    return Object.entries(groups).map(([category, categoryItems]) => ({
        category,
        itemCount: categoryItems.length,
        totalValue: calculateInventoryValue(categoryItems),
    }));
}

/**
 * Calculate stock turnover rate
 * @param movements - Array of stock movements
 * @param item - Inventory item
 * @param periodDays - Period in days (default: 30)
 * @returns Turnover rate
 */
export function calculateStockTurnover(
    movements: StockMovement[],
    item: InventoryItem,
    periodDays: number = 30
): number {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - periodDays);

    const recentMovements = movements.filter(
        movement =>
            movement.inventoryItemId === item.id &&
            movement.type === 'out' &&
            new Date(movement.timestamp) >= cutoffDate
    );

    const totalSold = recentMovements.reduce((sum, movement) => sum + Math.abs(movement.quantity), 0);
    const averageStock = item.currentStock || 1; // Avoid division by zero

    return totalSold / averageStock;
}

/**
 * Get movement type display text
 * @param type - Movement type
 * @returns Display text
 */
export function getMovementTypeText(type: string): string {
    switch (type) {
        case 'in': return 'Stock In';
        case 'out': return 'Stock Out';
        case 'adjustment': return 'Adjustment';
        default: return type;
    }
}

/**
 * Get movement reason display text
 * @param reason - Movement reason
 * @returns Display text
 */
export function getMovementReasonText(reason: string): string {
    switch (reason) {
        case 'purchase': return 'Purchase';
        case 'sale': return 'Sale';
        case 'return': return 'Return';
        case 'damage': return 'Damage';
        case 'expired': return 'Expired';
        case 'transfer': return 'Transfer';
        case 'adjustment': return 'Adjustment';
        case 'initial': return 'Initial Stock';
        default: return reason;
    }
}

/**
 * Validate SKU format
 * @param sku - SKU to validate
 * @returns True if valid SKU format
 */
export function isValidSKU(sku: string): boolean {
    // Basic SKU validation - alphanumeric with hyphens
    const skuPattern = /^[A-Z0-9-]+$/i;
    return skuPattern.test(sku) && sku.length >= 3;
}

/**
 * Validate barcode format
 * @param barcode - Barcode to validate
 * @returns True if valid barcode format
 */
export function isValidBarcode(barcode: string): boolean {
    // Basic barcode validation - numeric
    const barcodePattern = /^\d{8,13}$/;
    return barcodePattern.test(barcode);
} 