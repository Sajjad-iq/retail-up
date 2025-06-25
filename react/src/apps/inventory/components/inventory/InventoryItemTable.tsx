import React from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import {
    Edit,
    Package,
    AlertTriangle,
    CheckCircle,
    XCircle
} from 'lucide-react';

import { formatPrice, formatDate, getStockStatus } from '../../lib/utils/inventory-utils';
import type { InventoryItem } from '../../types/inventory';

interface InventoryItemTableProps {
    items: InventoryItem[];
    onEdit?: (item: InventoryItem) => void;
    onStockAdjust?: (item: InventoryItem) => void;
}

/**
 * InventoryItemTable Component
 * 
 * Displays inventory items in a table format with sortable columns and action buttons.
 * Provides detailed view of inventory information including pricing, stock levels, and supplier details.
 */
export function InventoryItemTable({ items, onEdit, onStockAdjust }: InventoryItemTableProps) {
    /**
     * Get stock status indicator for table display
     */
    const getStockStatusIndicator = (item: InventoryItem) => {
        const status = getStockStatus(item);

        switch (status) {
            case 'out':
                return <span title="Out of Stock"><XCircle className="h-4 w-4 text-red-500" /></span>;
            case 'low':
                return <span title="Low Stock"><AlertTriangle className="h-4 w-4 text-orange-500" /></span>;
            case 'overstocked':
                return <span title="Overstocked"><AlertTriangle className="h-4 w-4 text-blue-500" /></span>;
            default:
                return <span title="Normal Stock"><CheckCircle className="h-4 w-4 text-green-500" /></span>;
        }
    };

    /**
     * Get status badge for product status
     */
    const getProductStatusBadge = (status: string) => {
        switch (status) {
            case 'active':
                return <Badge className="text-xs">Active</Badge>;
            case 'inactive':
                return <Badge variant="secondary" className="text-xs">Inactive</Badge>;
            case 'discontinued':
                return <Badge variant="destructive" className="text-xs">Discontinued</Badge>;
            default:
                return <Badge variant="outline" className="text-xs">{status}</Badge>;
        }
    };

    const handleEdit = (item: InventoryItem) => {
        onEdit?.(item);
    };

    const handleStockAdjust = (item: InventoryItem) => {
        onStockAdjust?.(item);
    };

    return (
        <div className="rounded-md border">
            <Table>
                <TableHeader>
                    <TableRow>
                        <TableHead className="w-[30px]">Status</TableHead>
                        <TableHead className="min-w-[200px]">Item</TableHead>
                        <TableHead>SKU</TableHead>
                        <TableHead>Category</TableHead>
                        <TableHead className="text-right">Stock</TableHead>
                        <TableHead className="text-right">Cost</TableHead>
                        <TableHead className="text-right">Selling</TableHead>
                        <TableHead>Location</TableHead>
                        <TableHead>Supplier</TableHead>
                        <TableHead>Status</TableHead>
                        <TableHead>Updated</TableHead>
                        <TableHead className="text-right">Actions</TableHead>
                    </TableRow>
                </TableHeader>
                <TableBody>
                    {items.map((item) => (
                        <TableRow key={item.id} className="hover:bg-muted/50">
                            <TableCell>
                                {getStockStatusIndicator(item)}
                            </TableCell>

                            <TableCell>
                                <div>
                                    <div className="font-medium text-sm">{item.name}</div>
                                    {item.description && (
                                        <div className="text-xs text-muted-foreground line-clamp-1">
                                            {item.description}
                                        </div>
                                    )}
                                </div>
                            </TableCell>

                            <TableCell className="font-mono text-xs">{item.sku}</TableCell>

                            <TableCell>
                                <Badge variant="secondary" className="text-xs">
                                    {item.category}
                                </Badge>
                            </TableCell>

                            <TableCell className="text-right">
                                <div className="text-sm">
                                    <span className={
                                        getStockStatus(item) === 'out' ? 'text-red-600' :
                                            getStockStatus(item) === 'low' ? 'text-orange-600' :
                                                'text-foreground'
                                    }>
                                        {item.currentStock}
                                    </span>
                                    <span className="text-muted-foreground text-xs">
                                        /{item.minimumStock}
                                    </span>
                                </div>
                            </TableCell>

                            <TableCell className="text-right text-sm">
                                {formatPrice(item.costPrice)}
                            </TableCell>

                            <TableCell className="text-right text-sm font-medium">
                                {formatPrice(item.sellingPrice)}
                            </TableCell>

                            <TableCell className="text-xs text-muted-foreground">
                                {item.location || '-'}
                            </TableCell>

                            <TableCell className="text-xs">
                                {item.supplier ? (
                                    <div>
                                        <div className="font-medium">{item.supplier.name}</div>
                                        {item.supplier.contactPerson && (
                                            <div className="text-muted-foreground">
                                                {item.supplier.contactPerson}
                                            </div>
                                        )}
                                    </div>
                                ) : (
                                    <span className="text-muted-foreground">-</span>
                                )}
                            </TableCell>

                            <TableCell>
                                {getProductStatusBadge(item.status)}
                            </TableCell>

                            <TableCell className="text-xs text-muted-foreground">
                                {formatDate(item.updatedAt)}
                            </TableCell>

                            <TableCell className="text-right">
                                <div className="flex items-center gap-1 justify-end">
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={() => handleEdit(item)}
                                        className="h-7 px-2"
                                        aria-label={`Edit ${item.name}`}
                                    >
                                        <Edit className="h-3 w-3" />
                                    </Button>

                                    <Button
                                        size="sm"
                                        onClick={() => handleStockAdjust(item)}
                                        disabled={item.status !== 'active'}
                                        className="h-7 px-2"
                                        aria-label={`Adjust stock for ${item.name}`}
                                    >
                                        <Package className="h-3 w-3" />
                                    </Button>
                                </div>
                            </TableCell>
                        </TableRow>
                    ))}
                </TableBody>
            </Table>
        </div>
    );
} 