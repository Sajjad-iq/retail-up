import { describe, it, expect } from 'vitest';
import {
    formatPrice,
    formatTime,
    formatDate,
    generateTransactionId,
    calculateTax,
    calculateTotal,
    hasAvailableStock,
    isLowStock,
    isOutOfStock,
} from '../lib/utils/pos-utils';

describe('POS Utilities', () => {
    describe('formatPrice', () => {
        it('should format positive numbers correctly', () => {
            expect(formatPrice(10)).toBe('$10.00');
            expect(formatPrice(10.5)).toBe('$10.50');
            expect(formatPrice(10.99)).toBe('$10.99');
        });

        it('should format zero correctly', () => {
            expect(formatPrice(0)).toBe('$0.00');
        });

        it('should round to 2 decimal places', () => {
            expect(formatPrice(10.999)).toBe('$11.00');
            expect(formatPrice(10.996)).toBe('$11.00');
            expect(formatPrice(10.994)).toBe('$10.99');
        });
    });

    describe('formatTime', () => {
        it('should format time correctly', () => {
            const date = new Date('2023-01-01T14:30:00');
            const formatted = formatTime(date);

            // The exact format depends on locale, but should include hours and minutes
            expect(formatted).toMatch(/\d{1,2}:\d{2}/);
        });

        it('should handle different times', () => {
            const morning = new Date('2023-01-01T09:15:00');
            const evening = new Date('2023-01-01T21:45:00');

            expect(formatTime(morning)).toBeTruthy();
            expect(formatTime(evening)).toBeTruthy();
        });
    });

    describe('formatDate', () => {
        it('should format date correctly', () => {
            const date = new Date('2023-01-01');
            const formatted = formatDate(date);

            expect(formatted).toBeTruthy();
            expect(typeof formatted).toBe('string');
        });
    });

    describe('generateTransactionId', () => {
        it('should generate unique transaction IDs', () => {
            const id1 = generateTransactionId();
            const id2 = generateTransactionId();

            expect(id1).not.toBe(id2);
            expect(id1).toMatch(/^TXN-\d+-[a-z0-9]+$/);
            expect(id2).toMatch(/^TXN-\d+-[a-z0-9]+$/);
        });

        it('should include TXN prefix', () => {
            const id = generateTransactionId();
            expect(id).toMatch(/^TXN-/);
        });
    });

    describe('calculateTax', () => {
        it('should calculate tax with default rate (8%)', () => {
            expect(calculateTax(100)).toBe(8);
            expect(calculateTax(50)).toBe(4);
            expect(calculateTax(25)).toBe(2);
        });

        it('should calculate tax with custom rate', () => {
            expect(calculateTax(100, 0.10)).toBe(10); // 10%
            expect(calculateTax(100, 0.05)).toBe(5);  // 5%
            expect(calculateTax(100, 0.15)).toBe(15); // 15%
        });

        it('should handle zero subtotal', () => {
            expect(calculateTax(0)).toBe(0);
            expect(calculateTax(0, 0.10)).toBe(0);
        });
    });

    describe('calculateTotal', () => {
        it('should calculate total with default tax rate', () => {
            const subtotal = 100;
            const expectedTotal = subtotal + (subtotal * 0.08);
            expect(calculateTotal(subtotal)).toBe(expectedTotal);
        });

        it('should calculate total with custom tax rate', () => {
            const subtotal = 100;
            const taxRate = 0.10;
            const expectedTotal = subtotal + (subtotal * taxRate);
            expect(calculateTotal(subtotal, taxRate)).toBe(expectedTotal);
        });

        it('should handle zero subtotal', () => {
            expect(calculateTotal(0)).toBe(0);
            expect(calculateTotal(0, 0.15)).toBe(0);
        });
    });

    describe('hasAvailableStock', () => {
        it('should return true when stock is sufficient', () => {
            expect(hasAvailableStock(10, 5)).toBe(true);
            expect(hasAvailableStock(10, 10)).toBe(true);
            expect(hasAvailableStock(1, 1)).toBe(true);
        });

        it('should return false when stock is insufficient', () => {
            expect(hasAvailableStock(5, 10)).toBe(false);
            expect(hasAvailableStock(0, 1)).toBe(false);
            expect(hasAvailableStock(3, 5)).toBe(false);
        });
    });

    describe('isLowStock', () => {
        it('should return true for low stock with default threshold', () => {
            expect(isLowStock(1)).toBe(true);
            expect(isLowStock(3)).toBe(true);
            expect(isLowStock(5)).toBe(true);
        });

        it('should return false for adequate stock with default threshold', () => {
            expect(isLowStock(6)).toBe(false);
            expect(isLowStock(10)).toBe(false);
            expect(isLowStock(100)).toBe(false);
        });

        it('should return false for out of stock', () => {
            expect(isLowStock(0)).toBe(false);
        });

        it('should work with custom threshold', () => {
            expect(isLowStock(8, 10)).toBe(true);
            expect(isLowStock(10, 10)).toBe(true);
            expect(isLowStock(11, 10)).toBe(false);
        });
    });

    describe('isOutOfStock', () => {
        it('should return true when stock is zero', () => {
            expect(isOutOfStock(0)).toBe(true);
        });

        it('should return false when stock is available', () => {
            expect(isOutOfStock(1)).toBe(false);
            expect(isOutOfStock(5)).toBe(false);
            expect(isOutOfStock(100)).toBe(false);
        });
    });
}); 