import { useMemo } from 'react';
import { useInventoryStore } from '../store/inventory-store';
import {
    formatPrice,
    formatCurrency,
    formatDate,
    formatDateTime,
    filterItemsByQuery,
    sortItems,
    calculateInventoryValue,
    calculateRetailValue,
    getStockStatus,
    getCategoryBreakdown
} from '../lib/utils/inventory-utils';
import type {
    InventoryItem,
    InventoryFilters,
    StockAdjustmentFormData,
    InventoryItemFormData
} from '../types/inventory';

/**
 * Custom hook for inventory items operations
 * Provides inventory items state and operations with computed values
 */
export function useInventoryItems() {
    const {
        items,
        loading,
        addItem,
        updateItem,
        deleteItem,
        getItemById,
        getTotalInventoryValue,
    } = useInventoryStore();

    const computedValues = useMemo(() => ({
        totalItems: items.length,
        totalValue: getTotalInventoryValue(),
        formattedTotalValue: formatCurrency(getTotalInventoryValue()),
        activeItems: items.filter(item => item.status === 'active'),
        inactiveItems: items.filter(item => item.status === 'inactive'),
        discontinuedItems: items.filter(item => item.status === 'discontinued'),
    }), [items, getTotalInventoryValue]);

    const filterItems = (filters: InventoryFilters) => {
        let filteredItems = [...items];

        // Apply search query filter
        if (filters.query) {
            filteredItems = filterItemsByQuery(filteredItems, filters.query);
        }

        // Apply category filter
        if (filters.category && filters.category !== 'all') {
            filteredItems = filteredItems.filter(item => item.category === filters.category);
        }

        // Apply status filter
        if (filters.status) {
            filteredItems = filteredItems.filter(item => item.status === filters.status);
        }

        // Apply supplier filter
        if (filters.supplier) {
            filteredItems = filteredItems.filter(item => item.supplier?.id === filters.supplier);
        }

        // Apply stock level filter
        if (filters.stockLevel && filters.stockLevel !== 'all') {
            const stockStatus = filters.stockLevel;
            filteredItems = filteredItems.filter(item => getStockStatus(item) === stockStatus);
        }

        // Apply sorting
        if (filters.sortBy) {
            filteredItems = sortItems(
                filteredItems,
                filters.sortBy,
                filters.sortOrder || 'asc'
            );
        }

        return filteredItems;
    };

    return {
        items,
        ...computedValues,
        loading: loading.items || loading.saving,
        addItem,
        updateItem,
        deleteItem,
        getItemById,
        filterItems,
    };
}

/**
 * Custom hook for stock movements operations
 * Provides stock movements state and adjustment functionality
 */
export function useStockMovements() {
    const {
        movements,
        loading,
        adjustStock,
        getRecentMovements,
    } = useInventoryStore();

    const computedValues = useMemo(() => {
        const recentMovements = getRecentMovements(10);
        const todayMovements = movements.filter(movement => {
            const today = new Date().toDateString();
            return new Date(movement.timestamp).toDateString() === today;
        });

        return {
            recentMovements,
            todayMovements,
            totalMovements: movements.length,
            stockInToday: todayMovements.filter(m => m.type === 'in').length,
            stockOutToday: todayMovements.filter(m => m.type === 'out').length,
            adjustmentsToday: todayMovements.filter(m => m.type === 'adjustment').length,
        };
    }, [movements, getRecentMovements]);

    const adjustStockWithValidation = async (adjustmentData: StockAdjustmentFormData) => {
        try {
            const movement = await adjustStock(adjustmentData);
            return { success: true, movement };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Stock adjustment failed'
            };
        }
    };

    return {
        movements,
        ...computedValues,
        loading: loading.movements || loading.saving,
        adjustStock: adjustStockWithValidation,
    };
}

/**
 * Custom hook for low stock alerts operations
 * Provides alerts state and management functionality
 */
export function useInventoryAlerts() {
    const {
        alerts,
        items,
        acknowledgeAlert,
        refreshAlerts,
        getLowStockItems,
        getOutOfStockItems,
    } = useInventoryStore();

    const computedValues = useMemo(() => {
        const unacknowledgedAlerts = alerts.filter(alert => !alert.acknowledged);
        const criticalAlerts = alerts.filter(alert => alert.severity === 'critical');
        const warningAlerts = alerts.filter(alert => alert.severity === 'warning');

        return {
            totalAlerts: alerts.length,
            unacknowledgedCount: unacknowledgedAlerts.length,
            criticalCount: criticalAlerts.length,
            warningCount: warningAlerts.length,
            lowStockItems: getLowStockItems(),
            outOfStockItems: getOutOfStockItems(),
            lowStockCount: getLowStockItems().length,
            outOfStockCount: getOutOfStockItems().length,
        };
    }, [alerts, getLowStockItems, getOutOfStockItems]);

    return {
        alerts,
        ...computedValues,
        acknowledgeAlert,
        refreshAlerts,
    };
}

/**
 * Custom hook for categories operations
 * Provides categories state and management functionality
 */
export function useInventoryCategories() {
    const {
        categories,
        items,
        addCategory,
        updateCategory,
        deleteCategory,
        getCategoryById,
        getItemsByCategory,
    } = useInventoryStore();

    const computedValues = useMemo(() => {
        const categoryBreakdown = getCategoryBreakdown(items);
        const categoriesWithCounts = categories.map(category => ({
            ...category,
            itemCount: items.filter(item => item.category === category.name).length,
            totalValue: calculateInventoryValue(items.filter(item => item.category === category.name)),
        }));

        return {
            totalCategories: categories.length,
            categoryBreakdown,
            categoriesWithCounts,
        };
    }, [categories, items]);

    const deleteCategoryWithValidation = (id: string) => {
        const itemsInCategory = items.filter(item => {
            const category = getCategoryById(id);
            return category && item.category === category.name;
        });

        if (itemsInCategory.length > 0) {
            throw new Error(`Cannot delete category. ${itemsInCategory.length} items are still assigned to this category.`);
        }

        deleteCategory(id);
    };

    return {
        categories,
        ...computedValues,
        addCategory,
        updateCategory,
        deleteCategory: deleteCategoryWithValidation,
        getCategoryById,
        getItemsByCategory,
    };
}

/**
 * Custom hook for suppliers operations
 * Provides suppliers state and management functionality
 */
export function useInventorySuppliers() {
    const {
        suppliers,
        items,
        addSupplier,
        updateSupplier,
        deleteSupplier,
        getSupplierById,
    } = useInventoryStore();

    const computedValues = useMemo(() => {
        const suppliersWithCounts = suppliers.map(supplier => ({
            ...supplier,
            itemCount: items.filter(item => item.supplier?.id === supplier.id).length,
            totalValue: calculateInventoryValue(items.filter(item => item.supplier?.id === supplier.id)),
        }));

        return {
            totalSuppliers: suppliers.length,
            suppliersWithCounts,
        };
    }, [suppliers, items]);

    const deleteSupplierWithValidation = (id: string) => {
        const itemsWithSupplier = items.filter(item => item.supplier?.id === id);

        if (itemsWithSupplier.length > 0) {
            throw new Error(`Cannot delete supplier. ${itemsWithSupplier.length} items are still assigned to this supplier.`);
        }

        deleteSupplier(id);
    };

    return {
        suppliers,
        ...computedValues,
        addSupplier,
        updateSupplier,
        deleteSupplier: deleteSupplierWithValidation,
        getSupplierById,
    };
}

/**
 * Custom hook for inventory analytics
 * Provides computed analytics and statistics
 */
export function useInventoryAnalytics() {
    const { items, movements } = useInventoryStore();

    const analytics = useMemo(() => {
        const totalInventoryValue = calculateInventoryValue(items);
        const totalRetailValue = calculateRetailValue(items);
        const totalPotentialProfit = totalRetailValue - totalInventoryValue;

        // Stock status breakdown
        const stockStatusBreakdown = items.reduce((acc, item) => {
            const status = getStockStatus(item);
            acc[status] = (acc[status] || 0) + 1;
            return acc;
        }, {} as Record<string, number>);

        // Category breakdown
        const categoryBreakdown = getCategoryBreakdown(items);

        // Recent movements analysis
        const last30Days = new Date();
        last30Days.setDate(last30Days.getDate() - 30);

        const recentMovements = movements.filter(
            movement => new Date(movement.timestamp) >= last30Days
        );

        const movementsByType = recentMovements.reduce((acc, movement) => {
            acc[movement.type] = (acc[movement.type] || 0) + Math.abs(movement.quantity);
            return acc;
        }, {} as Record<string, number>);

        // Top items by value
        const topItemsByValue = [...items]
            .sort((a, b) => (b.sellingPrice * b.currentStock) - (a.sellingPrice * a.currentStock))
            .slice(0, 10)
            .map(item => ({
                ...item,
                totalValue: item.sellingPrice * item.currentStock,
            }));

        return {
            totalInventoryValue: formatCurrency(totalInventoryValue),
            totalRetailValue: formatCurrency(totalRetailValue),
            totalPotentialProfit: formatCurrency(totalPotentialProfit),
            profitMargin: totalInventoryValue > 0 ?
                ((totalPotentialProfit / totalInventoryValue) * 100).toFixed(1) + '%' : '0%',
            totalItems: items.length,
            stockStatusBreakdown,
            categoryBreakdown,
            movementsByType,
            topItemsByValue,
            recentMovementsCount: recentMovements.length,
        };
    }, [items, movements]);

    return analytics;
}

/**
 * Custom hook for formatting utilities
 * Provides formatting functions for the inventory system
 */
export function useInventoryFormatters() {
    return {
        formatPrice,
        formatCurrency,
        formatDate,
        formatDateTime,
        formatPercentage: (value: number) => `${value.toFixed(1)}%`,
        formatQuantity: (quantity: number) => quantity.toLocaleString(),
        formatStockStatus: (item: InventoryItem) => {
            const status = getStockStatus(item);
            const statusMap = {
                out: 'Out of Stock',
                low: 'Low Stock',
                normal: 'Normal',
                overstocked: 'Overstocked'
            };
            return statusMap[status];
        },
    };
}