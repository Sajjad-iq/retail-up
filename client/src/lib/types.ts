export interface Product {
    id: string;
    name: string;
    price: number;
    category: string;
    image?: string;
    barcode?: string;
    stock: number;
}

export interface CartItem {
    product: Product;
    quantity: number;
}

export interface Customer {
    id: string;
    name: string;
    email?: string;
    phone?: string;
}

export interface Transaction {
    id: string;
    items: CartItem[];
    customer?: Customer;
    subtotal: number;
    tax: number;
    total: number;
    paymentMethod: 'cash' | 'card' | 'mobile';
    timestamp: Date;
}

export type PaymentMethod = 'cash' | 'card' | 'mobile'; 