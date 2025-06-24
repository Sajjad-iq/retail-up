import { renderHook, act } from '@testing-library/react';
import { describe, it, expect, beforeEach } from 'vitest';
import { usePOSStore } from '../store/pos-store';
import type { Product } from '../types/pos';

// Mock product for testing
const mockProduct: Product = {
    id: 'test-1',
    name: 'Test Product',
    price: 10.00,
    category: 'Test',
    stock: 5,
    barcode: 'TEST123'
};

describe('POS Store', () => {
    beforeEach(() => {
        // Reset store state before each test
        const { result } = renderHook(() => usePOSStore());
        act(() => {
            result.current.clearCart();
        });
    });

    describe('Cart Operations', () => {
        it('should add product to cart', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
            });

            expect(result.current.cart).toHaveLength(1);
            expect(result.current.cart[0].product.id).toBe(mockProduct.id);
            expect(result.current.cart[0].quantity).toBe(1);
        });

        it('should increase quantity when adding existing product', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
                result.current.addToCart(mockProduct);
            });

            expect(result.current.cart).toHaveLength(1);
            expect(result.current.cart[0].quantity).toBe(2);
        });

        it('should add product with custom quantity', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 3);
            });

            expect(result.current.cart).toHaveLength(1);
            expect(result.current.cart[0].quantity).toBe(3);
        });

        it('should remove product from cart', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
                result.current.removeFromCart(mockProduct.id);
            });

            expect(result.current.cart).toHaveLength(0);
        });

        it('should update product quantity in cart', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
                result.current.updateCartQuantity(mockProduct.id, 5);
            });

            expect(result.current.cart[0].quantity).toBe(5);
        });

        it('should remove product when quantity is set to 0', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
                result.current.updateCartQuantity(mockProduct.id, 0);
            });

            expect(result.current.cart).toHaveLength(0);
        });

        it('should clear entire cart', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
                result.current.addToCart({ ...mockProduct, id: 'test-2' });
                result.current.clearCart();
            });

            expect(result.current.cart).toHaveLength(0);
        });
    });

    describe('Cart Calculations', () => {
        it('should calculate cart subtotal correctly', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 2); // 2 * $10.00 = $20.00
            });

            expect(result.current.getCartSubtotal()).toBe(20.00);
        });

        it('should calculate cart tax correctly', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 2); // Subtotal: $20.00
            });

            const expectedTax = 20.00 * 0.08; // 8% tax
            expect(result.current.getCartTax()).toBe(expectedTax);
        });

        it('should calculate cart total correctly', () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 2); // Subtotal: $20.00
            });

            const subtotal = 20.00;
            const tax = subtotal * 0.08;
            const expectedTotal = subtotal + tax;

            expect(result.current.getCartTotal()).toBe(expectedTotal);
        });

        it('should return zero for empty cart calculations', () => {
            const { result } = renderHook(() => usePOSStore());

            expect(result.current.getCartSubtotal()).toBe(0);
            expect(result.current.getCartTax()).toBe(0);
            expect(result.current.getCartTotal()).toBe(0);
        });
    });

    describe('Transaction Operations', () => {
        it('should process payment and create transaction', async () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 2);
            });

            let transaction: any;
            await act(async () => {
                transaction = await result.current.processPayment('cash');
            });

            expect(transaction).toBeDefined();
            expect(transaction!.paymentMethod).toBe('cash');
            expect(transaction!.total).toBe(result.current.getCartTotal());
            expect(transaction!.items).toHaveLength(1);
            expect(result.current.cart).toHaveLength(0); // Cart should be cleared
            expect(result.current.transactions).toHaveLength(1);
        });

        it('should add transaction to history', async () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
            });

            await act(async () => {
                await result.current.processPayment('card');
            });

            expect(result.current.transactions).toHaveLength(1);
            expect(result.current.transactions[0].paymentMethod).toBe('card');
        });
    });

    describe('Analytics Functions', () => {
        it('should get todays transactions correctly', async () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct);
            });

            await act(async () => {
                await result.current.processPayment('cash');
            });

            const todaysTransactions = result.current.getTodaysTransactions();
            expect(todaysTransactions).toHaveLength(1);
        });

        it('should calculate todays revenue correctly', async () => {
            const { result } = renderHook(() => usePOSStore());

            act(() => {
                result.current.addToCart(mockProduct, 2);
            });

            const expectedTotal = result.current.getCartTotal();

            await act(async () => {
                await result.current.processPayment('cash');
            });

            const todaysRevenue = result.current.getTodaysRevenue();
            expect(todaysRevenue).toBe(expectedTotal);
        });
    });

    describe('Customer Operations', () => {
        it('should set current customer', () => {
            const { result } = renderHook(() => usePOSStore());
            const customer = {
                id: 'customer-1',
                name: 'John Doe',
                email: 'john@example.com'
            };

            act(() => {
                result.current.setCurrentCustomer(customer);
            });

            expect(result.current.currentCustomer).toEqual(customer);
        });

        it('should clear current customer', () => {
            const { result } = renderHook(() => usePOSStore());
            const customer = {
                id: 'customer-1',
                name: 'John Doe'
            };

            act(() => {
                result.current.setCurrentCustomer(customer);
                result.current.setCurrentCustomer(null);
            });

            expect(result.current.currentCustomer).toBe(null);
        });
    });
}); 