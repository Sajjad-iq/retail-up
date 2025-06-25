import { create } from 'zustand';
import { devtools, persist } from 'zustand/middleware';
import type {
    InventoryItem,
    Category,
    Supplier,
    StockMovement,
    LowStockAlert,
    InventoryItemFormData,
    StockAdjustmentFormData,
    InventoryFilters,
    ProductStatus
} from '../types/inventory';
import {
    generateItemId,
    generateMovementId,
    generateSKU,
    formatDate,
    isLowStock,
    isOutOfStock,
    getLowStockAlerts
} from '../lib/utils/inventory-utils';

/**
 * Sample categories data for demonstration
 */
const sampleCategories: Category[] = [
    {
        id: 'cat-1',
        name: 'Electronics',
        description: 'Electronic devices and accessories',
        color: '#3B82F6'
    },
    {
        id: 'cat-2',
        name: 'Clothing',
        description: 'Apparel and fashion items',
        color: '#8B5CF6'
    },
    {
        id: 'cat-3',
        name: 'Home & Garden',
        description: 'Home improvement and garden supplies',
        color: '#10B981'
    },
    {
        id: 'cat-4',
        name: 'Books',
        description: 'Books and educational materials',
        color: '#F59E0B'
    }
];

/**
 * Sample suppliers data for demonstration
 */
const sampleSuppliers: Supplier[] = [
    {
        id: 'sup-1',
        name: 'TechCorp Supplies',
        contactPerson: 'John Smith',
        email: 'john@techcorp.com',
        phone: '+1-555-0123',
        address: '123 Tech Street, Silicon Valley, CA',
        paymentTerms: 'Net 30'
    },
    {
        id: 'sup-2',
        name: 'Fashion World Ltd',
        contactPerson: 'Sarah Johnson',
        email: 'sarah@fashionworld.com',
        phone: '+1-555-0456',
        address: '456 Fashion Ave, New York, NY',
        paymentTerms: 'Net 15'
    }
];

/**
 * Sample inventory items data for demonstration
 */
const sampleInventoryItems: InventoryItem[] = [
    {
        id: 'item-1',
        name: 'Wireless Bluetooth Headphones',
        description: 'High-quality wireless headphones with noise cancellation',
        category: 'Electronics',
        sku: 'ELE-001-WBH',
        barcode: '1234567890123',
        costPrice: 45.00,
        sellingPrice: 79.99,
        currentStock: 25,
        minimumStock: 5,
        maximumStock: 100,
        reorderQuantity: 30,
        location: 'A1-B2',
        supplier: sampleSuppliers[0],
        status: 'active',
        createdAt: new Date('2024-01-15'),
        updatedAt: new Date('2024-01-20')
    },
    {
        id: 'item-2',
        name: 'Cotton T-Shirt - Blue',
        description: '100% cotton t-shirt in blue color, size M',
        category: 'Clothing',
        sku: 'CLO-002-CTB',
        barcode: '2345678901234',
        costPrice: 8.50,
        sellingPrice: 19.99,
        currentStock: 3,
        minimumStock: 10,
        maximumStock: 200,
        reorderQuantity: 50,
        location: 'B2-C3',
        supplier: sampleSuppliers[1],
        status: 'active',
        createdAt: new Date('2024-01-10'),
        updatedAt: new Date('2024-01-18')
    },
    {
        id: 'item-3',
        name: 'Smart Home LED Bulb',
        description: 'WiFi-enabled LED bulb with app control',
        category: 'Electronics',
        sku: 'ELE-003-SHB',
        barcode: '3456789012345',
        costPrice: 12.00,
        sellingPrice: 24.99,
        currentStock: 0,
        minimumStock: 8,
        maximumStock: 80,
        reorderQuantity: 25,
        location: 'A2-B1',
        supplier: sampleSuppliers[0],
        status: 'active',
        createdAt: new Date('2024-01-12'),
        updatedAt: new Date('2024-01-19')
    }
];

interface InventoryStore {
    // State
    items: InventoryItem[];
    categories: Category[];
    suppliers: Supplier[];
    movements: StockMovement[];
    alerts: LowStockAlert[];
    loading: {
        items: boolean;
        movements: boolean;
        saving: boolean;
    };

    // Item Actions
    addItem: (formData: InventoryItemFormData) => Promise<InventoryItem>;
    updateItem: (id: string, formData: Partial<InventoryItemFormData>) => Promise<InventoryItem>;
    deleteItem: (id: string) => Promise<void>;

    // Stock Actions
    adjustStock: (adjustmentData: StockAdjustmentFormData) => Promise<StockMovement>;

    // Category Actions
    addCategory: (category: Omit<Category, 'id'>) => Category;
    updateCategory: (id: string, updates: Partial<Category>) => Category;
    deleteCategory: (id: string) => void;

    // Supplier Actions
    addSupplier: (supplier: Omit<Supplier, 'id'>) => Supplier;
    updateSupplier: (id: string, updates: Partial<Supplier>) => Supplier;
    deleteSupplier: (id: string) => void;

    // Alert Actions
    acknowledgeAlert: (alertId: string) => void;
    refreshAlerts: () => void;

    // Utility Actions
    getItemById: (id: string) => InventoryItem | undefined;
    getCategoryById: (id: string) => Category | undefined;
    getSupplierById: (id: string) => Supplier | undefined;

    // Computed getters
    getTotalInventoryValue: () => number;
    getLowStockItems: () => InventoryItem[];
    getOutOfStockItems: () => InventoryItem[];
    getItemsByCategory: (categoryId: string) => InventoryItem[];
    getRecentMovements: (limit?: number) => StockMovement[];
}

/**
 * Main Inventory store using Zustand
 * Provides state management for inventory items, categories, suppliers, and stock movements
 */
export const useInventoryStore = create<InventoryStore>()(
    devtools(
        persist(
            (set, get) => ({
                // Initial State
                items: sampleInventoryItems,
                categories: sampleCategories,
                suppliers: sampleSuppliers,
                movements: [],
                alerts: [],
                loading: {
                    items: false,
                    movements: false,
                    saving: false,
                },

                // Item Actions
                /**
                 * Add a new inventory item
                 */
                addItem: async (formData: InventoryItemFormData): Promise<InventoryItem> => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const category = state.getCategoryById(formData.categoryId);
                            const supplier = formData.supplierId ? state.getSupplierById(formData.supplierId) : undefined;

                            const newItem: InventoryItem = {
                                id: generateItemId(),
                                name: formData.name,
                                description: formData.description,
                                category: category?.name || 'Uncategorized',
                                sku: formData.sku || generateSKU(category?.name),
                                barcode: formData.barcode,
                                costPrice: formData.costPrice,
                                sellingPrice: formData.sellingPrice,
                                currentStock: formData.currentStock,
                                minimumStock: formData.minimumStock,
                                maximumStock: formData.maximumStock,
                                reorderQuantity: formData.reorderQuantity,
                                location: formData.location,
                                supplier: supplier,
                                status: formData.status,
                                createdAt: new Date(),
                                updatedAt: new Date(),
                            };

                            // Create initial stock movement if currentStock > 0
                            let newMovements = state.movements;
                            if (formData.currentStock > 0) {
                                const initialMovement: StockMovement = {
                                    id: generateMovementId(),
                                    inventoryItemId: newItem.id,
                                    type: 'in',
                                    reason: 'initial',
                                    quantity: formData.currentStock,
                                    previousStock: 0,
                                    newStock: formData.currentStock,
                                    reference: 'Initial Stock',
                                    notes: 'Initial stock entry',
                                    timestamp: new Date(),
                                };
                                newMovements = [initialMovement, ...newMovements];
                            }

                            set(state => ({
                                items: [...state.items, newItem],
                                movements: newMovements,
                                loading: { ...state.loading, saving: false },
                            }));

                            // Refresh alerts
                            get().refreshAlerts();

                            resolve(newItem);
                        }, 500); // Simulate API delay
                    });
                },

                /**
                 * Update an existing inventory item
                 */
                updateItem: async (id: string, formData: Partial<InventoryItemFormData>): Promise<InventoryItem> => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve, reject) => {
                        setTimeout(() => {
                            const state = get();
                            const existingItem = state.getItemById(id);

                            if (!existingItem) {
                                set(state => ({ loading: { ...state.loading, saving: false } }));
                                reject(new Error('Item not found'));
                                return;
                            }

                            const category = formData.categoryId ? state.getCategoryById(formData.categoryId) : undefined;
                            const supplier = formData.supplierId ? state.getSupplierById(formData.supplierId) : existingItem.supplier;

                            const updatedItem: InventoryItem = {
                                ...existingItem,
                                name: formData.name ?? existingItem.name,
                                description: formData.description ?? existingItem.description,
                                category: category?.name ?? existingItem.category,
                                sku: formData.sku ?? existingItem.sku,
                                barcode: formData.barcode ?? existingItem.barcode,
                                costPrice: formData.costPrice ?? existingItem.costPrice,
                                sellingPrice: formData.sellingPrice ?? existingItem.sellingPrice,
                                minimumStock: formData.minimumStock ?? existingItem.minimumStock,
                                maximumStock: formData.maximumStock ?? existingItem.maximumStock,
                                reorderQuantity: formData.reorderQuantity ?? existingItem.reorderQuantity,
                                location: formData.location ?? existingItem.location,
                                supplier: supplier,
                                status: formData.status ?? existingItem.status,
                                updatedAt: new Date(),
                            };

                            set(state => ({
                                items: state.items.map(item => item.id === id ? updatedItem : item),
                                loading: { ...state.loading, saving: false },
                            }));

                            // Refresh alerts
                            get().refreshAlerts();

                            resolve(updatedItem);
                        }, 500);
                    });
                },

                /**
                 * Delete an inventory item
                 */
                deleteItem: async (id: string): Promise<void> => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve, reject) => {
                        setTimeout(() => {
                            const state = get();
                            const item = state.getItemById(id);

                            if (!item) {
                                set(state => ({ loading: { ...state.loading, saving: false } }));
                                reject(new Error('Item not found'));
                                return;
                            }

                            set(state => ({
                                items: state.items.filter(item => item.id !== id),
                                movements: state.movements.filter(movement => movement.inventoryItemId !== id),
                                alerts: state.alerts.filter(alert => alert.inventoryItem.id !== id),
                                loading: { ...state.loading, saving: false },
                            }));

                            resolve();
                        }, 300);
                    });
                },

                // Stock Actions
                /**
                 * Adjust stock for an inventory item
                 */
                adjustStock: async (adjustmentData: StockAdjustmentFormData): Promise<StockMovement> => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve, reject) => {
                        setTimeout(() => {
                            const state = get();
                            const item = state.getItemById(adjustmentData.itemId);

                            if (!item) {
                                set(state => ({ loading: { ...state.loading, saving: false } }));
                                reject(new Error('Item not found'));
                                return;
                            }

                            const newStock = Math.max(0, item.currentStock + adjustmentData.quantity);
                            const movementType = adjustmentData.quantity > 0 ? 'in' : 'out';

                            const movement: StockMovement = {
                                id: generateMovementId(),
                                inventoryItemId: item.id,
                                type: movementType,
                                reason: adjustmentData.reason,
                                quantity: adjustmentData.quantity,
                                previousStock: item.currentStock,
                                newStock: newStock,
                                reference: adjustmentData.reference,
                                notes: adjustmentData.notes,
                                timestamp: new Date(),
                            };

                            const updatedItem = {
                                ...item,
                                currentStock: newStock,
                                updatedAt: new Date(),
                            };

                            set(state => ({
                                items: state.items.map(i => i.id === item.id ? updatedItem : i),
                                movements: [movement, ...state.movements],
                                loading: { ...state.loading, saving: false },
                            }));

                            // Refresh alerts
                            get().refreshAlerts();

                            resolve(movement);
                        }, 400);
                    });
                },

                // Category Actions
                /**
                 * Add a new category
                 */
                addCategory: (category: Omit<Category, 'id'>): Category => {
                    const newCategory: Category = {
                        id: `cat-${Date.now()}`,
                        ...category,
                    };

                    set(state => ({
                        categories: [...state.categories, newCategory],
                    }));

                    return newCategory;
                },

                /**
                 * Update an existing category
                 */
                updateCategory: (id: string, updates: Partial<Category>): Category => {
                    let updatedCategory: Category | null = null;

                    set(state => ({
                        categories: state.categories.map(cat => {
                            if (cat.id === id) {
                                updatedCategory = { ...cat, ...updates };
                                return updatedCategory;
                            }
                            return cat;
                        }),
                    }));

                    if (!updatedCategory) {
                        throw new Error('Category not found');
                    }

                    return updatedCategory;
                },

                /**
                 * Delete a category
                 */
                deleteCategory: (id: string): void => {
                    set(state => ({
                        categories: state.categories.filter(cat => cat.id !== id),
                    }));
                },

                // Supplier Actions
                /**
                 * Add a new supplier
                 */
                addSupplier: (supplier: Omit<Supplier, 'id'>): Supplier => {
                    const newSupplier: Supplier = {
                        id: `sup-${Date.now()}`,
                        ...supplier,
                    };

                    set(state => ({
                        suppliers: [...state.suppliers, newSupplier],
                    }));

                    return newSupplier;
                },

                /**
                 * Update an existing supplier
                 */
                updateSupplier: (id: string, updates: Partial<Supplier>): Supplier => {
                    let updatedSupplier: Supplier | null = null;

                    set(state => ({
                        suppliers: state.suppliers.map(sup => {
                            if (sup.id === id) {
                                updatedSupplier = { ...sup, ...updates };
                                return updatedSupplier;
                            }
                            return sup;
                        }),
                    }));

                    if (!updatedSupplier) {
                        throw new Error('Supplier not found');
                    }

                    return updatedSupplier;
                },

                /**
                 * Delete a supplier
                 */
                deleteSupplier: (id: string): void => {
                    set(state => ({
                        suppliers: state.suppliers.filter(sup => sup.id !== id),
                    }));
                },

                // Alert Actions
                /**
                 * Acknowledge an alert
                 */
                acknowledgeAlert: (alertId: string): void => {
                    set(state => ({
                        alerts: state.alerts.map(alert =>
                            alert.id === alertId ? { ...alert, acknowledged: true } : alert
                        ),
                    }));
                },

                /**
                 * Refresh alerts based on current inventory
                 */
                refreshAlerts: (): void => {
                    const state = get();
                    const newAlerts = getLowStockAlerts(state.items);

                    set({ alerts: newAlerts });
                },

                // Utility Actions
                /**
                 * Get an item by ID
                 */
                getItemById: (id: string): InventoryItem | undefined => {
                    return get().items.find(item => item.id === id);
                },

                /**
                 * Get a category by ID
                 */
                getCategoryById: (id: string): Category | undefined => {
                    return get().categories.find(cat => cat.id === id);
                },

                /**
                 * Get a supplier by ID
                 */
                getSupplierById: (id: string): Supplier | undefined => {
                    return get().suppliers.find(sup => sup.id === id);
                },

                // Computed getters
                /**
                 * Get total inventory value
                 */
                getTotalInventoryValue: (): number => {
                    const state = get();
                    return state.items.reduce((total, item) =>
                        total + (item.costPrice * item.currentStock), 0
                    );
                },

                /**
                 * Get low stock items
                 */
                getLowStockItems: (): InventoryItem[] => {
                    return get().items.filter(item => isLowStock(item));
                },

                /**
                 * Get out of stock items
                 */
                getOutOfStockItems: (): InventoryItem[] => {
                    return get().items.filter(item => isOutOfStock(item));
                },

                /**
                 * Get items by category
                 */
                getItemsByCategory: (categoryId: string): InventoryItem[] => {
                    const state = get();
                    const category = state.getCategoryById(categoryId);
                    if (!category) return [];

                    return state.items.filter(item => item.category === category.name);
                },

                /**
                 * Get recent movements
                 */
                getRecentMovements: (limit: number = 10): StockMovement[] => {
                    const movements = get().movements;
                    return movements
                        .sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime())
                        .slice(0, limit);
                },
            }),
            {
                name: 'inventory-storage',
                partialize: (state) => ({
                    items: state.items,
                    categories: state.categories,
                    suppliers: state.suppliers,
                    movements: state.movements,
                }),
            }
        ),
        {
            name: 'inventory-store',
        }
    )
);