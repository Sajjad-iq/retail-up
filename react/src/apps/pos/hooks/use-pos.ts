import { useMemo } from 'react';
import { usePOSStore } from '../store/pos-store';
import { formatPrice, formatTime, calculateTax, calculateTotal } from '../lib/utils/pos-utils';
import type { Product, PaymentMethod } from '../types/pos';

/**
 * Custom hook for cart operations
 * Provides cart state and operations with computed values
 */
export function useCart() {
    const {
        cart,
        addToCart,
        removeFromCart,
        updateCartQuantity,
        clearCart,
        getCartSubtotal,
        getCartTax,
        getCartTotal,
    } = usePOSStore();

    const computedValues = useMemo(() => ({
        subtotal: getCartSubtotal(),
        tax: getCartTax(),
        total: getCartTotal(),
        itemCount: cart.reduce((sum, item) => sum + item.quantity, 0),
        isEmpty: cart.length === 0,
        formattedSubtotal: formatPrice(getCartSubtotal()),
        formattedTax: formatPrice(getCartTax()),
        formattedTotal: formatPrice(getCartTotal()),
    }), [cart, getCartSubtotal, getCartTax, getCartTotal]);

    return {
        cart,
        ...computedValues,
        addToCart,
        removeFromCart,
        updateCartQuantity,
        clearCart,
    };
}

/**
 * Custom hook for product operations
 * Provides product state and filtering functionality
 */
export function useProducts() {
    const { products } = usePOSStore();

    const getProductsByCategory = (category: string) => {
        if (category === 'All') return products;
        return products.filter(product => product.category === category);
    };

    const searchProducts = (query: string) => {
        if (!query.trim()) return products;

        const searchTerm = query.toLowerCase();
        return products.filter(product =>
            product.name.toLowerCase().includes(searchTerm) ||
            product.category.toLowerCase().includes(searchTerm) ||
            product.barcode?.includes(searchTerm)
        );
    };

    const getProductById = (id: string) => {
        return products.find(product => product.id === id);
    };

    const categories = useMemo(() => {
        const uniqueCategories = [...new Set(products.map(product => product.category))];
        return ['All', ...uniqueCategories];
    }, [products]);

    return {
        products,
        categories,
        getProductsByCategory,
        searchProducts,
        getProductById,
    };
}

/**
 * Custom hook for transaction operations
 * Provides transaction history and payment processing
 */
export function useTransactions() {
    const {
        transactions,
        processPayment,
        getTodaysTransactions,
        getTodaysRevenue,
    } = usePOSStore();

    const todaysTransactions = useMemo(() => getTodaysTransactions(), [transactions, getTodaysTransactions]);
    const todaysRevenue = useMemo(() => getTodaysRevenue(), [transactions, getTodaysRevenue]);

    const recentTransactions = useMemo(() => {
        return todaysTransactions.slice(0, 5);
    }, [todaysTransactions]);

    const processPaymentWithValidation = async (paymentMethod: PaymentMethod) => {
        try {
            const transaction = await processPayment(paymentMethod);
            return { success: true, transaction };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Payment failed'
            };
        }
    };

    return {
        transactions,
        todaysTransactions,
        recentTransactions,
        todaysRevenue: formatPrice(todaysRevenue),
        todaysSalesCount: todaysTransactions.length,
        processPayment: processPaymentWithValidation,
    };
}

/**
 * Custom hook for POS analytics
 * Provides computed analytics and statistics
 */
export function usePOSAnalytics() {
    const { transactions } = usePOSStore();

    const analytics = useMemo(() => {
        const today = new Date().toDateString();
        const todaysTransactions = transactions.filter(
            t => new Date(t.timestamp).toDateString() === today
        );

        const totalRevenue = todaysTransactions.reduce((sum, t) => sum + t.total, 0);
        const totalTax = todaysTransactions.reduce((sum, t) => sum + t.tax, 0);
        const averageOrderValue = todaysTransactions.length > 0
            ? totalRevenue / todaysTransactions.length
            : 0;

        // Payment method breakdown
        const paymentMethods = todaysTransactions.reduce((acc, t) => {
            acc[t.paymentMethod] = (acc[t.paymentMethod] || 0) + 1;
            return acc;
        }, {} as Record<string, number>);

        // Top selling products
        const productSales = todaysTransactions.reduce((acc, t) => {
            t.items.forEach(item => {
                const productName = item.product.name;
                acc[productName] = (acc[productName] || 0) + item.quantity;
            });
            return acc;
        }, {} as Record<string, number>);

        const topProducts = Object.entries(productSales)
            .sort(([, a], [, b]) => b - a)
            .slice(0, 5)
            .map(([name, quantity]) => ({ name, quantity }));

        return {
            totalRevenue: formatPrice(totalRevenue),
            totalTax: formatPrice(totalTax),
            averageOrderValue: formatPrice(averageOrderValue),
            transactionCount: todaysTransactions.length,
            paymentMethods,
            topProducts,
        };
    }, [transactions]);

    return analytics;
}

/**
 * Custom hook for formatting utilities
 * Provides formatting functions for the POS system
 */
export function usePOSFormatters() {
    return {
        formatPrice,
        formatTime,
        calculateTax,
        calculateTotal,
        formatDateTime: (date: Date) => `${date.toLocaleDateString()} ${formatTime(date)}`,
    };
} 