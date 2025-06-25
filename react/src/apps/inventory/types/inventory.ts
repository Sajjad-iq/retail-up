/**
 * Inventory item interface representing products in the inventory system
 */
export interface InventoryItem {
    /** Unique identifier for the inventory item */
    id: string;
    /** Display name of the product */
    name: string;
    /** Product description */
    description?: string;
    /** Product category for filtering */
    category: string;
    /** Product SKU code */
    sku: string;
    /** Product barcode for scanning */
    barcode?: string;
    /** Cost price per unit */
    costPrice: number;
    /** Selling price per unit */
    sellingPrice: number;
    /** Current stock quantity */
    currentStock: number;
    /** Minimum stock level for reorder alerts */
    minimumStock: number;
    /** Maximum stock capacity */
    maximumStock: number;
    /** Reorder quantity */
    reorderQuantity: number;
    /** Storage location */
    location?: string;
    /** Supplier information */
    supplier?: Supplier;
    /** Product image URL */
    image?: string;
    /** Product status */
    status: ProductStatus;
    /** Date when item was created */
    createdAt: Date;
    /** Date when item was last updated */
    updatedAt: Date;
}

/**
 * Supplier information
 */
export interface Supplier {
    /** Unique supplier identifier */
    id: string;
    /** Supplier company name */
    name: string;
    /** Contact person name */
    contactPerson?: string;
    /** Email address */
    email?: string;
    /** Phone number */
    phone?: string;
    /** Supplier address */
    address?: string;
    /** Payment terms */
    paymentTerms?: string;
}

/**
 * Product status types
 */
export type ProductStatus = 'active' | 'inactive' | 'discontinued';

/**
 * Stock movement types
 */
export type MovementType = 'in' | 'out' | 'adjustment';

/**
 * Stock movement reason types
 */
export type MovementReason =
    | 'purchase'
    | 'sale'
    | 'return'
    | 'damage'
    | 'expired'
    | 'transfer'
    | 'adjustment'
    | 'initial';

/**
 * Stock movement record
 */
export interface StockMovement {
    /** Unique movement identifier */
    id: string;
    /** Related inventory item */
    inventoryItemId: string;
    /** Movement type */
    type: MovementType;
    /** Reason for movement */
    reason: MovementReason;
    /** Quantity changed (positive for in, negative for out) */
    quantity: number;
    /** Stock level before movement */
    previousStock: number;
    /** Stock level after movement */
    newStock: number;
    /** Reference number (PO, invoice, etc.) */
    reference?: string;
    /** Additional notes */
    notes?: string;
    /** User who made the movement */
    userId?: string;
    /** Movement timestamp */
    timestamp: Date;
}

/**
 * Low stock alert
 */
export interface LowStockAlert {
    /** Alert identifier */
    id: string;
    /** Related inventory item */
    inventoryItem: InventoryItem;
    /** Alert severity */
    severity: 'warning' | 'critical';
    /** Alert message */
    message: string;
    /** Alert creation date */
    createdAt: Date;
    /** Whether alert is acknowledged */
    acknowledged: boolean;
}

/**
 * Inventory category
 */
export interface Category {
    /** Unique category identifier */
    id: string;
    /** Category name */
    name: string;
    /** Category description */
    description?: string;
    /** Parent category ID for nested categories */
    parentId?: string;
    /** Category color for UI */
    color?: string;
}

/**
 * Inventory report data
 */
export interface InventoryReport {
    /** Total items count */
    totalItems: number;
    /** Total inventory value */
    totalValue: number;
    /** Low stock items count */
    lowStockCount: number;
    /** Out of stock items count */
    outOfStockCount: number;
    /** Categories breakdown */
    categoriesBreakdown: CategoryBreakdown[];
    /** Recent movements count */
    recentMovementsCount: number;
}

/**
 * Category breakdown for reports
 */
export interface CategoryBreakdown {
    /** Category name */
    category: string;
    /** Items count in category */
    itemCount: number;
    /** Total value in category */
    totalValue: number;
}

/**
 * Inventory state interface
 */
export interface InventoryState {
    /** Inventory items */
    items: InventoryItem[];
    /** Categories */
    categories: Category[];
    /** Suppliers */
    suppliers: Supplier[];
    /** Stock movements */
    movements: StockMovement[];
    /** Low stock alerts */
    alerts: LowStockAlert[];
    /** Loading states */
    loading: {
        items: boolean;
        movements: boolean;
        saving: boolean;
    };
}

/**
 * Form data for creating/editing inventory items
 */
export interface InventoryItemFormData {
    /** Product name */
    name: string;
    /** Product description */
    description?: string;
    /** Category ID */
    categoryId: string;
    /** SKU code */
    sku: string;
    /** Barcode */
    barcode?: string;
    /** Cost price */
    costPrice: number;
    /** Selling price */
    sellingPrice: number;
    /** Current stock */
    currentStock: number;
    /** Minimum stock */
    minimumStock: number;
    /** Maximum stock */
    maximumStock: number;
    /** Reorder quantity */
    reorderQuantity: number;
    /** Storage location */
    location?: string;
    /** Supplier ID */
    supplierId?: string;
    /** Product status */
    status: ProductStatus;
}

/**
 * Form data for stock adjustments
 */
export interface StockAdjustmentFormData {
    /** Inventory item ID */
    itemId: string;
    /** Adjustment quantity (can be positive or negative) */
    quantity: number;
    /** Reason for adjustment */
    reason: MovementReason;
    /** Reference number */
    reference?: string;
    /** Notes */
    notes?: string;
}

/**
 * Search filters for inventory
 */
export interface InventoryFilters {
    /** Search query */
    query?: string;
    /** Category filter */
    category?: string;
    /** Status filter */
    status?: ProductStatus;
    /** Supplier filter */
    supplier?: string;
    /** Stock level filter */
    stockLevel?: 'all' | 'low' | 'out' | 'normal';
    /** Sort by field */
    sortBy?: 'name' | 'stock' | 'price' | 'updatedAt';
    /** Sort direction */
    sortOrder?: 'asc' | 'desc';
} 