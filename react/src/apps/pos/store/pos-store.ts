import { create } from 'zustand';
import { devtools, persist } from 'zustand/middleware';
import type { Product, CartItem, Customer, Transaction, PaymentMethod } from '../types/pos';

/**
 * Sample products data for demonstration
 */
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

interface POSStore {
    // State
    products: Product[];
    cart: CartItem[];
    currentCustomer: Customer | null;
    transactions: Transaction[];

    // Cart Actions
    addToCart: (product: Product, quantity?: number) => void;
    removeFromCart: (productId: string) => void;
    updateCartQuantity: (productId: string, quantity: number) => void;
    clearCart: () => void;

    // Customer Actions
    setCurrentCustomer: (customer: Customer | null) => void;

    // Transaction Actions
    processPayment: (paymentMethod: PaymentMethod) => Promise<Transaction>;

    // Computed getters
    getCartTotal: () => number;
    getCartSubtotal: () => number;
    getCartTax: () => number;
    getTodaysTransactions: () => Transaction[];
    getTodaysRevenue: () => number;
}

/**
 * Main POS store using Zustand
 * Provides state management for products, cart, customers, and transactions
 */
export const usePOSStore = create<POSStore>()(
    devtools(
        persist(
            (set, get) => ({
                // Initial State
                products: sampleProducts,
                cart: [],
                currentCustomer: null,
                transactions: [],

                // Cart Actions
                /**
                 * Add a product to the cart or increase quantity if it exists
                 */
                addToCart: (product: Product, quantity = 1) => {
                    set((state) => {
                        const existingItem = state.cart.find(item => item.product.id === product.id);

                        if (existingItem) {
                            return {
                                cart: state.cart.map(item =>
                                    item.product.id === product.id
                                        ? { ...item, quantity: item.quantity + quantity }
                                        : item
                                )
                            };
                        } else {
                            return {
                                cart: [...state.cart, { product, quantity }]
                            };
                        }
                    });
                },

                /**
                 * Remove a product from the cart
                 */
                removeFromCart: (productId: string) => {
                    set((state) => ({
                        cart: state.cart.filter(item => item.product.id !== productId)
                    }));
                },

                /**
                 * Update the quantity of a product in the cart
                 */
                updateCartQuantity: (productId: string, quantity: number) => {
                    set((state) => {
                        if (quantity <= 0) {
                            return {
                                cart: state.cart.filter(item => item.product.id !== productId)
                            };
                        }

                        return {
                            cart: state.cart.map(item =>
                                item.product.id === productId
                                    ? { ...item, quantity }
                                    : item
                            )
                        };
                    });
                },

                /**
                 * Clear all items from the cart
                 */
                clearCart: () => {
                    set({ cart: [] });
                },

                // Customer Actions
                /**
                 * Set the current customer
                 */
                setCurrentCustomer: (customer: Customer | null) => {
                    set({ currentCustomer: customer });
                },

                // Transaction Actions
                /**
                 * Process payment and create a transaction
                 */
                processPayment: async (paymentMethod: PaymentMethod): Promise<Transaction> => {
                    return new Promise((resolve) => {
                        const state = get();
                        const subtotal = state.getCartSubtotal();
                        const tax = state.getCartTax();
                        const total = state.getCartTotal();

                        const transaction: Transaction = {
                            id: `TXN-${Date.now()}`,
                            items: [...state.cart],
                            customer: state.currentCustomer,
                            subtotal,
                            tax,
                            total,
                            paymentMethod,
                            timestamp: new Date()
                        };

                        set((state) => ({
                            transactions: [transaction, ...state.transactions],
                            cart: [],
                            currentCustomer: null
                        }));

                        resolve(transaction);
                    });
                },

                // Computed getters
                /**
                 * Calculate cart subtotal
                 */
                getCartSubtotal: () => {
                    const { cart } = get();
                    return cart.reduce((sum, item) => sum + (item.product.price * item.quantity), 0);
                },

                /**
                 * Calculate cart tax (8%)
                 */
                getCartTax: () => {
                    const subtotal = get().getCartSubtotal();
                    return subtotal * 0.08;
                },

                /**
                 * Calculate cart total (subtotal + tax)
                 */
                getCartTotal: () => {
                    const state = get();
                    return state.getCartSubtotal() + state.getCartTax();
                },

                /**
                 * Get today's transactions
                 */
                getTodaysTransactions: () => {
                    const { transactions } = get();
                    const today = new Date().toDateString();
                    return transactions.filter(
                        transaction => new Date(transaction.timestamp).toDateString() === today
                    );
                },

                /**
                 * Calculate today's revenue
                 */
                getTodaysRevenue: () => {
                    const todaysTransactions = get().getTodaysTransactions();
                    return todaysTransactions.reduce((sum, transaction) => sum + transaction.total, 0);
                }
            }),
            {
                name: 'pos-store',
                // Only persist transactions and customer data, not cart
                partialize: (state) => ({
                    transactions: state.transactions,
                    currentCustomer: state.currentCustomer
                })
            }
        ),
        {
            name: 'pos-store'
        }
    )
); 