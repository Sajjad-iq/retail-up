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
}

/**
 * Cart item representing a product with quantity
 */
export interface CartItem {
    /** Product details */
    product: Product;
    /** Quantity selected */
    quantity: number;
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
}

/**
 * Payment method types
 */
export type PaymentMethod = 'cash' | 'card' | 'mobile';

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
    /** Subtotal before tax */
    subtotal: number;
    /** Tax amount */
    tax: number;
    /** Total amount */
    total: number;
    /** Payment method used */
    paymentMethod: PaymentMethod;
    /** Transaction timestamp */
    timestamp: Date;
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
}

/**
 * Payment form data
 */
export interface PaymentFormData {
    /** Selected payment method */
    paymentMethod: PaymentMethod;
    /** Amount received (for cash payments) */
    amountReceived?: number;
} 