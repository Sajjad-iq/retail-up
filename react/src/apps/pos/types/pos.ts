/**
 * Product interface representing items available for sale
 */
export interface Product {
    /** Unique identifier for the product */
    id: string;
    /** Display name of the product */
    name: string;
    /** Price in USD */
    price: number;
    /** Product category for filtering */
    category: string;
    /** Optional product image URL */
    image?: string;
    /** Optional barcode for scanning */
    barcode?: string;
    /** Current stock quantity */
    stock: number;
    /** Tax rate for this product */
    taxRate?: number;
    /** Product description */
    description?: string;
    /** Product SKU */
    sku?: string;
}

/**
 * Cart item representing a product with quantity and discounts
 */
export interface CartItem {
    /** Product details */
    product: Product;
    /** Quantity selected */
    quantity: number;
    /** Applied discount amount */
    discount?: number;
    /** Discount type */
    discountType?: 'percentage' | 'fixed';
    /** Custom price override */
    customPrice?: number;
}

/**
 * Customer information
 */
export interface Customer {
    /** Unique customer identifier */
    id: string;
    /** Customer name */
    name: string;
    /** Optional email address */
    email?: string;
    /** Optional phone number */
    phone?: string;
    /** Customer address */
    address?: string;
    /** Loyalty points */
    loyaltyPoints?: number;
    /** Customer type */
    type?: 'regular' | 'vip' | 'wholesale';
}

/**
 * Discount interface
 */
export interface Discount {
    /** Unique discount identifier */
    id: string;
    /** Discount name */
    name: string;
    /** Discount type */
    type: 'percentage' | 'fixed' | 'bogo';
    /** Discount value */
    value: number;
    /** Minimum purchase amount */
    minAmount?: number;
    /** Valid from date */
    validFrom?: Date;
    /** Valid until date */
    validUntil?: Date;
    /** Applicable product categories */
    categories?: string[];
    /** Is the discount active */
    isActive: boolean;
}

/**
 * Payment method types
 */
export type PaymentMethod = 'cash' | 'card' | 'mobile' | 'check' | 'giftcard' | 'loyalty';

/**
 * Payment details for a transaction
 */
export interface PaymentDetail {
    /** Payment method */
    method: PaymentMethod;
    /** Amount paid with this method */
    amount: number;
    /** Reference number (for card/digital payments) */
    reference?: string;
    /** Change given (for cash payments) */
    change?: number;
}

/**
 * Transaction status
 */
export type TransactionStatus = 'completed' | 'pending' | 'cancelled' | 'refunded' | 'held';

/**
 * Transaction record
 */
export interface Transaction {
    /** Unique transaction identifier */
    id: string;
    /** Items purchased in this transaction */
    items: CartItem[];
    /** Optional customer information */
    customer?: Customer | null;
    /** Subtotal before tax and discounts */
    subtotal: number;
    /** Total discount amount */
    discount: number;
    /** Tax amount */
    tax: number;
    /** Total amount */
    total: number;
    /** Payment details (supports split payments) */
    payments: PaymentDetail[];
    /** Transaction timestamp */
    timestamp: Date;
    /** Transaction status */
    status: TransactionStatus;
    /** Cashier/User who processed the transaction */
    cashier?: string;
    /** Applied discounts */
    appliedDiscounts?: Discount[];
    /** Receipt number */
    receiptNumber?: string;
    /** Notes */
    notes?: string;
}

/**
 * Return/Refund transaction
 */
export interface ReturnTransaction {
    /** Unique return identifier */
    id: string;
    /** Original transaction ID */
    originalTransactionId: string;
    /** Items being returned */
    items: CartItem[];
    /** Return reason */
    reason: string;
    /** Refund amount */
    refundAmount: number;
    /** Refund method */
    refundMethod: PaymentMethod;
    /** Return timestamp */
    timestamp: Date;
    /** Who processed the return */
    processedBy: string;
}

/**
 * Held transaction (for later completion)
 */
export interface HeldTransaction {
    /** Unique hold identifier */
    id: string;
    /** Cart items */
    items: CartItem[];
    /** Customer information */
    customer?: Customer | null;
    /** Hold timestamp */
    timestamp: Date;
    /** Hold reason/note */
    note?: string;
    /** Who created the hold */
    createdBy: string;
}

/**
 * Barcode scan result
 */
export interface BarcodeScanResult {
    /** Scanned barcode */
    code: string;
    /** Associated product (if found) */
    product?: Product;
    /** Scan timestamp */
    timestamp: Date;
}

/**
 * Cash drawer operation
 */
export interface CashDrawerOperation {
    /** Operation ID */
    id: string;
    /** Operation type */
    type: 'open' | 'close' | 'payout' | 'payin';
    /** Amount involved */
    amount: number;
    /** Reason/description */
    reason: string;
    /** Timestamp */
    timestamp: Date;
    /** Who performed the operation */
    operator: string;
}

/**
 * Shift information
 */
export interface Shift {
    /** Shift ID */
    id: string;
    /** Cashier/operator */
    operator: string;
    /** Shift start time */
    startTime: Date;
    /** Shift end time */
    endTime?: Date;
    /** Starting cash amount */
    startingCash: number;
    /** Ending cash amount */
    endingCash?: number;
    /** Transactions during this shift */
    transactions: string[];
    /** Cash drawer operations */
    drawerOperations: CashDrawerOperation[];
    /** Shift notes */
    notes?: string;
}

/**
 * Receipt configuration
 */
export interface ReceiptConfig {
    /** Store name */
    storeName: string;
    /** Store address */
    storeAddress: string;
    /** Store phone */
    storePhone: string;
    /** Tax ID */
    taxId?: string;
    /** Receipt footer message */
    footerMessage?: string;
    /** Logo URL */
    logoUrl?: string;
}

/**
 * POS store state interface
 */
export interface POSState {
    /** Available products */
    products: Product[];
    /** Current cart items */
    cart: CartItem[];
    /** Current customer */
    currentCustomer: Customer | null;
    /** Transaction history */
    transactions: Transaction[];
    /** Return transactions */
    returns: ReturnTransaction[];
    /** Held transactions */
    heldTransactions: HeldTransaction[];
    /** Available discounts */
    discounts: Discount[];
    /** Current shift */
    currentShift: Shift | null;
    /** Cash drawer operations */
    drawerOperations: CashDrawerOperation[];
    /** Receipt configuration */
    receiptConfig: ReceiptConfig;
}

/**
 * Payment form data
 */
export interface PaymentFormData {
    /** Selected payment methods (for split payments) */
    payments: PaymentDetail[];
    /** Amount received (for cash payments) */
    amountReceived?: number;
    /** Customer information */
    customer?: Customer;
}

/**
 * Return form data
 */
export interface ReturnFormData {
    /** Original transaction ID */
    originalTransactionId: string;
    /** Items to return */
    items: { productId: string; quantity: number; reason: string }[];
    /** Overall return reason */
    reason: string;
    /** Refund method */
    refundMethod: PaymentMethod;
}

/**
 * Discount application result
 */
export interface DiscountResult {
    /** Was discount applied successfully */
    applied: boolean;
    /** Discount amount */
    amount: number;
    /** Error message if not applied */
    error?: string;
} 