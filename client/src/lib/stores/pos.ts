import { writable, get } from 'svelte/store';
import type { Product, CartItem, Customer, Transaction } from '../types.js';

// Sample products data
const sampleProducts: Product[] = [
    {
        id: '1',
        name: 'Coffee - Espresso',
        price: 2.50,
        category: 'Beverages',
        stock: 100,
        barcode: '123456789'
    },
    {
        id: '2',
        name: 'Coffee - Latte',
        price: 4.00,
        category: 'Beverages',
        stock: 80,
        barcode: '123456790'
    },
    {
        id: '3',
        name: 'Croissant',
        price: 3.25,
        category: 'Bakery',
        stock: 25,
        barcode: '123456791'
    },
    {
        id: '4',
        name: 'Sandwich - BLT',
        price: 8.50,
        category: 'Food',
        stock: 15,
        barcode: '123456792'
    },
    {
        id: '5',
        name: 'Water Bottle',
        price: 1.50,
        category: 'Beverages',
        stock: 200,
        barcode: '123456793'
    },
    {
        id: '6',
        name: 'Muffin - Blueberry',
        price: 2.75,
        category: 'Bakery',
        stock: 30,
        barcode: '123456794'
    }
];

// Stores
export const products = writable<Product[]>(sampleProducts);
export const cart = writable<CartItem[]>([]);
export const currentCustomer = writable<Customer | null>(null);
export const transactions = writable<Transaction[]>([]);

// Cart operations
export const cartOperations = {
    addItem: (product: Product, quantity: number = 1) => {
        cart.update(items => {
            const existingItem = items.find(item => item.product.id === product.id);
            if (existingItem) {
                existingItem.quantity += quantity;
                return items;
            } else {
                return [...items, { product, quantity }];
            }
        });
    },

    removeItem: (productId: string) => {
        cart.update(items => items.filter(item => item.product.id !== productId));
    },

    updateQuantity: (productId: string, quantity: number) => {
        cart.update(items => {
            if (quantity <= 0) {
                return items.filter(item => item.product.id !== productId);
            }
            return items.map(item =>
                item.product.id === productId
                    ? { ...item, quantity }
                    : item
            );
        });
    },

    clear: () => {
        cart.set([]);
    }
};

// Transaction operations
export const transactionOperations = {
    processPayment: (paymentMethod: 'cash' | 'card' | 'mobile') => {
        return new Promise<Transaction>((resolve) => {
            const items = get(cart);
            const subtotal = items.reduce((sum, item) => sum + (item.product.price * item.quantity), 0);
            const tax = subtotal * 0.08; // 8% tax
            const total = subtotal + tax;

            const transaction: Transaction = {
                id: `TXN-${Date.now()}`,
                items: [...items],
                subtotal,
                tax,
                total,
                paymentMethod,
                timestamp: new Date()
            };

            transactions.update(txns => [transaction, ...txns]);
            cartOperations.clear();
            resolve(transaction);
        });
    }
}; 