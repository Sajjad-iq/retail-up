import React, { useState, useEffect } from 'react';
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Plus, Package } from 'lucide-react';

import { useInventoryItems, useInventoryCategories, useInventorySuppliers } from '../../hooks/use-inventory';
import type { InventoryItemFormData, ProductStatus } from '../../types/inventory';

interface AddItemDialogProps {
    isOpen: boolean;
    onClose: () => void;
    onSuccess: () => void;
}

/**
 * AddItemDialog Component
 * 
 * Dialog for adding new inventory items.
 * Provides form interface for creating new inventory items with validation.
 */
export function AddItemDialog({ isOpen, onClose, onSuccess }: AddItemDialogProps) {
    const [formData, setFormData] = useState<InventoryItemFormData>({
        name: '',
        description: '',
        categoryId: '',
        sku: '',
        barcode: '',
        costPrice: 0,
        sellingPrice: 0,
        currentStock: 0,
        minimumStock: 5,
        maximumStock: 100,
        reorderQuantity: 20,
        location: '',
        supplierId: '',
        status: 'active'
    });

    const [errors, setErrors] = useState<Partial<Record<keyof InventoryItemFormData, string>>>({});
    const [isSubmitting, setIsSubmitting] = useState(false);

    const { addItem, loading } = useInventoryItems();
    const { categories } = useInventoryCategories();
    const { suppliers } = useInventorySuppliers();

    // Reset form when dialog opens/closes
    useEffect(() => {
        if (!isOpen) {
            setFormData({
                name: '',
                description: '',
                categoryId: '',
                sku: '',
                barcode: '',
                costPrice: 0,
                sellingPrice: 0,
                currentStock: 0,
                minimumStock: 5,
                maximumStock: 100,
                reorderQuantity: 20,
                location: '',
                supplierId: '',
                status: 'active'
            });
            setErrors({});
            setIsSubmitting(false);
        }
    }, [isOpen]);

    const validateForm = (): boolean => {
        const newErrors: Partial<Record<keyof InventoryItemFormData, string>> = {};

        if (!formData.name.trim()) {
            newErrors.name = 'Name is required';
        }

        if (!formData.categoryId) {
            newErrors.categoryId = 'Category is required';
        }

        if (!formData.sku.trim()) {
            newErrors.sku = 'SKU is required';
        }

        if (formData.costPrice < 0) {
            newErrors.costPrice = 'Cost price must be non-negative';
        }

        if (formData.sellingPrice < 0) {
            newErrors.sellingPrice = 'Selling price must be non-negative';
        }

        if (formData.sellingPrice < formData.costPrice) {
            newErrors.sellingPrice = 'Selling price should be greater than cost price';
        }

        if (formData.currentStock < 0) {
            newErrors.currentStock = 'Current stock must be non-negative';
        }

        if (formData.minimumStock < 0) {
            newErrors.minimumStock = 'Minimum stock must be non-negative';
        }

        if (formData.maximumStock <= formData.minimumStock) {
            newErrors.maximumStock = 'Maximum stock must be greater than minimum stock';
        }

        if (formData.reorderQuantity <= 0) {
            newErrors.reorderQuantity = 'Reorder quantity must be positive';
        }

        setErrors(newErrors);
        return Object.keys(newErrors).length === 0;
    };

    const handleInputChange = (field: keyof InventoryItemFormData, value: string | number) => {
        setFormData(prev => ({
            ...prev,
            [field]: value
        }));

        // Clear error for this field
        if (errors[field]) {
            setErrors(prev => ({
                ...prev,
                [field]: undefined
            }));
        }
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();

        if (!validateForm()) {
            return;
        }

        setIsSubmitting(true);

        try {
            await addItem(formData);
            onSuccess();
            onClose();
        } catch (error) {
            console.error('Failed to add item:', error);
            // You might want to show a toast error here
        } finally {
            setIsSubmitting(false);
        }
    };

    const handleClose = () => {
        if (!isSubmitting) {
            onClose();
        }
    };

    // Auto-generate SKU based on category and name
    const generateSKU = () => {
        if (formData.name && formData.categoryId) {
            const category = categories.find(c => c.id === formData.categoryId);
            if (category) {
                const categoryCode = category.name.substring(0, 3).toUpperCase();
                const nameCode = formData.name.substring(0, 3).toUpperCase();
                const randomNum = Math.floor(Math.random() * 1000).toString().padStart(3, '0');
                const sku = `${categoryCode}-${nameCode}-${randomNum}`;
                handleInputChange('sku', sku);
            }
        }
    };

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="sm:max-w-[600px] max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Plus className="h-5 w-5" />
                        Add New Item
                    </DialogTitle>
                    <DialogDescription>
                        Add a new item to your inventory with all necessary details.
                    </DialogDescription>
                </DialogHeader>

                <form onSubmit={handleSubmit} className="space-y-4">
                    {/* Basic Information */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="name">Product Name *</Label>
                            <Input
                                id="name"
                                value={formData.name}
                                onChange={(e) => handleInputChange('name', e.target.value)}
                                placeholder="Enter product name"
                                className={errors.name ? 'border-red-500' : ''}
                            />
                            {errors.name && <p className="text-sm text-red-500">{errors.name}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="category">Category *</Label>
                            <Select
                                value={formData.categoryId}
                                onValueChange={(value) => handleInputChange('categoryId', value)}
                            >
                                <SelectTrigger className={errors.categoryId ? 'border-red-500' : ''}>
                                    <SelectValue placeholder="Select category" />
                                </SelectTrigger>
                                <SelectContent>
                                    {categories.map((category) => (
                                        <SelectItem key={category.id} value={category.id}>
                                            {category.name}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                            {errors.categoryId && <p className="text-sm text-red-500">{errors.categoryId}</p>}
                        </div>
                    </div>

                    {/* SKU and Barcode */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="sku">SKU *</Label>
                            <div className="flex gap-2">
                                <Input
                                    id="sku"
                                    value={formData.sku}
                                    onChange={(e) => handleInputChange('sku', e.target.value)}
                                    placeholder="Enter SKU"
                                    className={errors.sku ? 'border-red-500' : ''}
                                />
                                <Button type="button" variant="outline" onClick={generateSKU}>
                                    <Package className="h-4 w-4" />
                                </Button>
                            </div>
                            {errors.sku && <p className="text-sm text-red-500">{errors.sku}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="barcode">Barcode</Label>
                            <Input
                                id="barcode"
                                value={formData.barcode || ''}
                                onChange={(e) => handleInputChange('barcode', e.target.value)}
                                placeholder="Enter barcode (optional)"
                            />
                        </div>
                    </div>

                    {/* Pricing */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="costPrice">Cost Price *</Label>
                            <Input
                                id="costPrice"
                                type="number"
                                step="0.01"
                                min="0"
                                value={formData.costPrice}
                                onChange={(e) => handleInputChange('costPrice', parseFloat(e.target.value) || 0)}
                                placeholder="0.00"
                                className={errors.costPrice ? 'border-red-500' : ''}
                            />
                            {errors.costPrice && <p className="text-sm text-red-500">{errors.costPrice}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="sellingPrice">Selling Price *</Label>
                            <Input
                                id="sellingPrice"
                                type="number"
                                step="0.01"
                                min="0"
                                value={formData.sellingPrice}
                                onChange={(e) => handleInputChange('sellingPrice', parseFloat(e.target.value) || 0)}
                                placeholder="0.00"
                                className={errors.sellingPrice ? 'border-red-500' : ''}
                            />
                            {errors.sellingPrice && <p className="text-sm text-red-500">{errors.sellingPrice}</p>}
                        </div>
                    </div>

                    {/* Stock Information */}
                    <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="currentStock">Current Stock</Label>
                            <Input
                                id="currentStock"
                                type="number"
                                min="0"
                                value={formData.currentStock}
                                onChange={(e) => handleInputChange('currentStock', parseInt(e.target.value) || 0)}
                                className={errors.currentStock ? 'border-red-500' : ''}
                            />
                            {errors.currentStock && <p className="text-sm text-red-500">{errors.currentStock}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="minimumStock">Min Stock</Label>
                            <Input
                                id="minimumStock"
                                type="number"
                                min="0"
                                value={formData.minimumStock}
                                onChange={(e) => handleInputChange('minimumStock', parseInt(e.target.value) || 0)}
                                className={errors.minimumStock ? 'border-red-500' : ''}
                            />
                            {errors.minimumStock && <p className="text-sm text-red-500">{errors.minimumStock}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="maximumStock">Max Stock</Label>
                            <Input
                                id="maximumStock"
                                type="number"
                                min="1"
                                value={formData.maximumStock}
                                onChange={(e) => handleInputChange('maximumStock', parseInt(e.target.value) || 0)}
                                className={errors.maximumStock ? 'border-red-500' : ''}
                            />
                            {errors.maximumStock && <p className="text-sm text-red-500">{errors.maximumStock}</p>}
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="reorderQuantity">Reorder Qty</Label>
                            <Input
                                id="reorderQuantity"
                                type="number"
                                min="1"
                                value={formData.reorderQuantity}
                                onChange={(e) => handleInputChange('reorderQuantity', parseInt(e.target.value) || 0)}
                                className={errors.reorderQuantity ? 'border-red-500' : ''}
                            />
                            {errors.reorderQuantity && <p className="text-sm text-red-500">{errors.reorderQuantity}</p>}
                        </div>
                    </div>

                    {/* Additional Information */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="supplier">Supplier</Label>
                            <Select
                                value={formData.supplierId || 'none'}
                                onValueChange={(value) => handleInputChange('supplierId', value === 'none' ? '' : value)}
                            >
                                <SelectTrigger>
                                    <SelectValue placeholder="Select supplier (optional)" />
                                </SelectTrigger>
                                <SelectContent>
                                    <SelectItem value="none">No supplier</SelectItem>
                                    {suppliers.map((supplier) => (
                                        <SelectItem key={supplier.id} value={supplier.id}>
                                            {supplier.name}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="location">Location</Label>
                            <Input
                                id="location"
                                value={formData.location || ''}
                                onChange={(e) => handleInputChange('location', e.target.value)}
                                placeholder="Storage location (optional)"
                            />
                        </div>
                    </div>

                    {/* Description and Status */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div className="space-y-2">
                            <Label htmlFor="description">Description</Label>
                            <Input
                                id="description"
                                value={formData.description || ''}
                                onChange={(e) => handleInputChange('description', e.target.value)}
                                placeholder="Product description (optional)"
                            />
                        </div>

                        <div className="space-y-2">
                            <Label htmlFor="status">Status</Label>
                            <Select
                                value={formData.status}
                                onValueChange={(value: ProductStatus) => handleInputChange('status', value)}
                            >
                                <SelectTrigger>
                                    <SelectValue />
                                </SelectTrigger>
                                <SelectContent>
                                    <SelectItem value="active">Active</SelectItem>
                                    <SelectItem value="inactive">Inactive</SelectItem>
                                    <SelectItem value="discontinued">Discontinued</SelectItem>
                                </SelectContent>
                            </Select>
                        </div>
                    </div>

                    <DialogFooter>
                        <Button type="button" variant="outline" onClick={handleClose} disabled={isSubmitting}>
                            Cancel
                        </Button>
                        <Button type="submit" disabled={isSubmitting || loading}>
                            {isSubmitting ? 'Adding...' : 'Add Item'}
                        </Button>
                    </DialogFooter>
                </form>
            </DialogContent>
        </Dialog>
    );
}