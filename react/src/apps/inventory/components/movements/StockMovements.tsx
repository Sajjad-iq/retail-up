import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Label } from '@/components/ui/label';
import {
    Package,
    TrendingUp,
    TrendingDown,
    Plus,
    Minus,
    AlertCircle,
    Calendar,
    User,
    FileText,
    Search,
    Filter
} from 'lucide-react';

import { useStockMovements, useInventoryItems, useInventoryFormatters } from '../../hooks/use-inventory';
import type { StockAdjustmentFormData, MovementReason, MovementType } from '../../types/inventory';

/**
 * StockMovements Component
 * 
 * Displays and manages stock movements and transactions.
 * Shows recent stock movements, allows creating new movements, and provides movement history.
 */
export function StockMovements() {
    const [showAdjustmentDialog, setShowAdjustmentDialog] = useState(false);
    const [filterType, setFilterType] = useState<MovementType | 'all'>('all');
    const [searchQuery, setSearchQuery] = useState('');
    const [adjustmentForm, setAdjustmentForm] = useState<StockAdjustmentFormData>({
        itemId: '',
        quantity: 0,
        reason: 'adjustment',
        reference: '',
        notes: ''
    });

    const {
        movements,
        recentMovements,
        todayMovements,
        stockInToday,
        stockOutToday,
        adjustmentsToday,
        loading,
        adjustStock
    } = useStockMovements();

    const { items, getItemById } = useInventoryItems();
    const { formatDateTime, formatQuantity } = useInventoryFormatters();

    const filteredMovements = movements.filter(movement => {
        const item = getItemById(movement.inventoryItemId);
        const matchesSearch = !searchQuery ||
            item?.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
            item?.sku.toLowerCase().includes(searchQuery.toLowerCase()) ||
            movement.reference?.toLowerCase().includes(searchQuery.toLowerCase());

        const matchesType = filterType === 'all' || movement.type === filterType;

        return matchesSearch && matchesType;
    });

    const handleAdjustmentSubmit = async (e: React.FormEvent) => {
        e.preventDefault();

        const result = await adjustStock(adjustmentForm);

        if (result.success) {
            setShowAdjustmentDialog(false);
            setAdjustmentForm({
                itemId: '',
                quantity: 0,
                reason: 'adjustment',
                reference: '',
                notes: ''
            });
        } else {
            alert(result.error || 'Failed to adjust stock');
        }
    };

    const getMovementIcon = (type: MovementType) => {
        switch (type) {
            case 'in':
                return <TrendingUp className="h-4 w-4 text-green-500" />;
            case 'out':
                return <TrendingDown className="h-4 w-4 text-red-500" />;
            case 'adjustment':
                return <AlertCircle className="h-4 w-4 text-blue-500" />;
            default:
                return <Package className="h-4 w-4" />;
        }
    };

    const getMovementTypeLabel = (type: MovementType) => {
        switch (type) {
            case 'in':
                return 'Stock In';
            case 'out':
                return 'Stock Out';
            case 'adjustment':
                return 'Adjustment';
            default:
                return type;
        }
    };

    const getReasonLabel = (reason: MovementReason) => {
        const reasonMap = {
            purchase: 'Purchase',
            sale: 'Sale',
            return: 'Return',
            damage: 'Damage',
            expired: 'Expired',
            transfer: 'Transfer',
            adjustment: 'Adjustment',
            initial: 'Initial Stock'
        };
        return reasonMap[reason] || reason;
    };

    if (loading) {
        return (
            <div className="flex items-center justify-center h-full">
                <div className="text-center">
                    <Package className="h-8 w-8 mx-auto mb-4 text-muted-foreground animate-pulse" />
                    <p className="text-muted-foreground">Loading movements...</p>
                </div>
            </div>
        );
    }

    return (
        <div className="h-full flex flex-col">
            {/* Header with Stats */}
            <div className="border-b bg-muted/20 p-4">
                <div className="flex items-center justify-between mb-4">
                    <div>
                        <h2 className="text-lg font-semibold">Stock Movements</h2>
                        <p className="text-sm text-muted-foreground">
                            Track and manage all inventory movements
                        </p>
                    </div>
                    <Dialog open={showAdjustmentDialog} onOpenChange={setShowAdjustmentDialog}>
                        <DialogTrigger asChild>
                            <Button>
                                <AlertCircle className="mr-2 h-4 w-4" />
                                Adjust Stock
                            </Button>
                        </DialogTrigger>
                        <DialogContent>
                            <form onSubmit={handleAdjustmentSubmit}>
                                <DialogHeader>
                                    <DialogTitle>Stock Adjustment</DialogTitle>
                                    <DialogDescription>
                                        Adjust inventory quantities with reason and notes
                                    </DialogDescription>
                                </DialogHeader>
                                <div className="grid gap-4 py-4">
                                    <div className="grid gap-2">
                                        <Label htmlFor="item">Item</Label>
                                        <Select
                                            value={adjustmentForm.itemId}
                                            onValueChange={(value) => setAdjustmentForm(prev => ({ ...prev, itemId: value }))}
                                            required
                                        >
                                            <SelectTrigger>
                                                <SelectValue placeholder="Select item" />
                                            </SelectTrigger>
                                            <SelectContent>
                                                {items.map((item) => (
                                                    <SelectItem key={item.id} value={item.id}>
                                                        {item.name} - {item.sku} (Stock: {item.currentStock})
                                                    </SelectItem>
                                                ))}
                                            </SelectContent>
                                        </Select>
                                    </div>
                                    <div className="grid gap-2">
                                        <Label htmlFor="quantity">Quantity</Label>
                                        <Input
                                            id="quantity"
                                            type="number"
                                            value={adjustmentForm.quantity}
                                            onChange={(e) => setAdjustmentForm(prev => ({
                                                ...prev,
                                                quantity: parseInt(e.target.value) || 0
                                            }))}
                                            placeholder="Enter quantity (+ or -)"
                                            required
                                        />
                                    </div>
                                    <div className="grid gap-2">
                                        <Label htmlFor="reason">Reason</Label>
                                        <Select
                                            value={adjustmentForm.reason}
                                            onValueChange={(value: MovementReason) =>
                                                setAdjustmentForm(prev => ({ ...prev, reason: value }))
                                            }
                                        >
                                            <SelectTrigger>
                                                <SelectValue />
                                            </SelectTrigger>
                                            <SelectContent>
                                                <SelectItem value="adjustment">Adjustment</SelectItem>
                                                <SelectItem value="damage">Damage</SelectItem>
                                                <SelectItem value="expired">Expired</SelectItem>
                                                <SelectItem value="return">Return</SelectItem>
                                                <SelectItem value="transfer">Transfer</SelectItem>
                                            </SelectContent>
                                        </Select>
                                    </div>
                                    <div className="grid gap-2">
                                        <Label htmlFor="reference">Reference</Label>
                                        <Input
                                            id="reference"
                                            value={adjustmentForm.reference || ''}
                                            onChange={(e) => setAdjustmentForm(prev => ({
                                                ...prev,
                                                reference: e.target.value
                                            }))}
                                            placeholder="Reference number (optional)"
                                        />
                                    </div>
                                    <div className="grid gap-2">
                                        <Label htmlFor="notes">Notes</Label>
                                        <Input
                                            id="notes"
                                            value={adjustmentForm.notes || ''}
                                            onChange={(e) => setAdjustmentForm(prev => ({
                                                ...prev,
                                                notes: e.target.value
                                            }))}
                                            placeholder="Additional notes (optional)"
                                        />
                                    </div>
                                </div>
                                <DialogFooter>
                                    <Button type="button" variant="outline" onClick={() => setShowAdjustmentDialog(false)}>
                                        Cancel
                                    </Button>
                                    <Button type="submit">
                                        Adjust Stock
                                    </Button>
                                </DialogFooter>
                            </form>
                        </DialogContent>
                    </Dialog>
                </div>

                {/* Today's Stats */}
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <TrendingUp className="h-4 w-4 text-green-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Stock In</p>
                                    <p className="text-lg font-semibold">{stockInToday}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <TrendingDown className="h-4 w-4 text-red-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Stock Out</p>
                                    <p className="text-lg font-semibold">{stockOutToday}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <AlertCircle className="h-4 w-4 text-blue-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Adjustments</p>
                                    <p className="text-lg font-semibold">{adjustmentsToday}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <Calendar className="h-4 w-4 text-muted-foreground" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Today's Total</p>
                                    <p className="text-lg font-semibold">{todayMovements.length}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                </div>
            </div>

            {/* Filters */}
            <div className="border-b bg-background p-4">
                <div className="flex items-center gap-4">
                    <div className="relative flex-1 max-w-sm">
                        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                        <Input
                            placeholder="Search movements..."
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="pl-9"
                        />
                    </div>
                    <Select value={filterType} onValueChange={(value: MovementType | 'all') => setFilterType(value)}>
                        <SelectTrigger className="w-[150px]">
                            <SelectValue placeholder="Filter by type" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Types</SelectItem>
                            <SelectItem value="in">Stock In</SelectItem>
                            <SelectItem value="out">Stock Out</SelectItem>
                            <SelectItem value="adjustment">Adjustments</SelectItem>
                        </SelectContent>
                    </Select>
                </div>
            </div>

            {/* Movements List */}
            <div className="flex-1 overflow-y-auto p-4">
                <div className="space-y-3">
                    {filteredMovements.length === 0 ? (
                        <div className="text-center py-12">
                            <Package className="h-12 w-12 mx-auto mb-4 text-muted-foreground" />
                            <p className="text-muted-foreground">No movements found</p>
                        </div>
                    ) : (
                        filteredMovements.map((movement) => {
                            const item = getItemById(movement.inventoryItemId);
                            return (
                                <Card key={movement.id}>
                                    <CardContent className="p-4">
                                        <div className="flex items-center justify-between">
                                            <div className="flex items-center gap-3">
                                                {getMovementIcon(movement.type)}
                                                <div>
                                                    <p className="font-medium">{item?.name || 'Unknown Item'}</p>
                                                    <p className="text-sm text-muted-foreground">
                                                        {item?.sku} • {getReasonLabel(movement.reason)}
                                                    </p>
                                                </div>
                                            </div>
                                            <div className="text-right">
                                                <div className="flex items-center gap-2">
                                                    <Badge variant={movement.type === 'in' ? 'default' : movement.type === 'out' ? 'destructive' : 'secondary'}>
                                                        {getMovementTypeLabel(movement.type)}
                                                    </Badge>
                                                    <span className={`font-semibold ${movement.type === 'in' ? 'text-green-600' :
                                                        movement.type === 'out' ? 'text-red-600' :
                                                            'text-blue-600'
                                                        }`}>
                                                        {movement.type === 'in' ? '+' : movement.type === 'out' ? '-' : ''}
                                                        {formatQuantity(Math.abs(movement.quantity))}
                                                    </span>
                                                </div>
                                                <p className="text-xs text-muted-foreground mt-1">
                                                    {formatDateTime(movement.timestamp)}
                                                </p>
                                            </div>
                                        </div>
                                        {movement.reference && (
                                            <div className="mt-2 pt-2 border-t">
                                                <p className="text-xs text-muted-foreground">
                                                    <FileText className="inline h-3 w-3 mr-1" />
                                                    Ref: {movement.reference}
                                                </p>
                                            </div>
                                        )}
                                        {movement.notes && (
                                            <div className="mt-1">
                                                <p className="text-xs text-muted-foreground">
                                                    Notes: {movement.notes}
                                                </p>
                                            </div>
                                        )}
                                    </CardContent>
                                </Card>
                            );
                        })
                    )}
                </div>
            </div>
        </div>
    );
}