import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
    Package,
    Edit,
    MoreVertical,
    AlertTriangle,
    CheckCircle,
    XCircle,
    Minus,
    Plus
} from 'lucide-react';

import { formatPrice, getStockStatus, isLowStock, isOutOfStock } from '../../lib/utils/inventory-utils';
import { useInventoryFormatters } from '../../hooks/use-inventory';
import type { InventoryItem } from '../../types/inventory';

interface InventoryItemCardProps {
    item: InventoryItem;
    onEdit?: (item: InventoryItem) => void;
    onStockAdjust?: (item: InventoryItem) => void;
}

/**
 * InventoryItemCard Component
 * 
 * Displays a single inventory item in a card format with stock status, pricing, and action buttons.
 * Shows item information including name, SKU, stock level, and pricing with appropriate status badges.
 */
export function InventoryItemCard({ item, onEdit, onStockAdjust }: InventoryItemCardProps) {
    const [isHovered, setIsHovered] = useState(false);
    const { formatStockStatus } = useInventoryFormatters();

    /**
     * Get stock status badge for an inventory item
     */
    const getStockBadge = (item: InventoryItem) => {
        const status = getStockStatus(item);

        switch (status) {
            case 'out':
                return (
                    <Badge variant="destructive" className="mt-2 text-xs">
                        <XCircle className="h-3 w-3 mr-1" />
                        Out of Stock
                    </Badge>
                );
            case 'low':
                return (
                    <Badge variant="outline" className="mt-2 text-xs border-orange-500 text-orange-600">
                        <AlertTriangle className="h-3 w-3 mr-1" />
                        Low Stock
                    </Badge>
                );
            case 'overstocked':
                return (
                    <Badge variant="outline" className="mt-2 text-xs border-blue-500 text-blue-600">
                        <AlertTriangle className="h-3 w-3 mr-1" />
                        Overstocked
                    </Badge>
                );
            default:
                return (
                    <Badge variant="outline" className="mt-2 text-xs border-green-500 text-green-600">
                        <CheckCircle className="h-3 w-3 mr-1" />
                        Normal
                    </Badge>
                );
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
                return null;
        }
    };

    const handleEdit = () => {
        onEdit?.(item);
    };

    const handleStockAdjust = () => {
        onStockAdjust?.(item);
    };

    return (
        <Card
            className="h-full hover:shadow-md transition-shadow cursor-pointer relative"
            onMouseEnter={() => setIsHovered(true)}
            onMouseLeave={() => setIsHovered(false)}
        >
            <CardHeader className="pb-2">
                <div className="flex items-center justify-between">
                    <div className="flex items-center gap-2">
                        <Badge variant="secondary" className="text-xs">
                            {item.category}
                        </Badge>
                        {getProductStatusBadge(item.status)}
                    </div>
                    <Package className="h-4 w-4 text-muted-foreground" />
                </div>

                <CardTitle className="text-sm line-clamp-2" title={item.name}>
                    {item.name}
                </CardTitle>

                <CardDescription className="text-xs space-y-1">
                    <div>SKU: {item.sku}</div>
                    <div>Stock: {item.currentStock} / {item.minimumStock} min</div>
                </CardDescription>
            </CardHeader>

            <CardContent className="pt-0 space-y-3">
                {/* Pricing Information */}
                <div className="space-y-1">
                    <div className="flex items-center justify-between text-xs">
                        <span className="text-muted-foreground">Cost:</span>
                        <span className="font-medium">{formatPrice(item.costPrice)}</span>
                    </div>
                    <div className="flex items-center justify-between">
                        <span className="text-xs text-muted-foreground">Selling:</span>
                        <span className="text-sm font-bold text-primary">
                            {formatPrice(item.sellingPrice)}
                        </span>
                    </div>
                </div>

                {/* Stock Status Badge */}
                {getStockBadge(item)}

                {/* Location */}
                {item.location && (
                    <div className="text-xs text-muted-foreground">
                        Location: {item.location}
                    </div>
                )}

                {/* Action Buttons - Show on hover */}
                {isHovered && (
                    <div className="absolute inset-0 bg-background/95 flex items-center justify-center gap-2 rounded-lg transition-opacity">
                        <Button
                            size="sm"
                            variant="outline"
                            onClick={handleEdit}
                            className="h-8"
                            aria-label={`Edit ${item.name}`}
                        >
                            <Edit className="h-4 w-4 mr-1" />
                            Edit
                        </Button>

                        <Button
                            size="sm"
                            onClick={handleStockAdjust}
                            disabled={item.status !== 'active'}
                            className="h-8"
                            aria-label={`Adjust stock for ${item.name}`}
                        >
                            <Package className="h-4 w-4 mr-1" />
                            Stock
                        </Button>
                    </div>
                )}
            </CardContent>
        </Card>
    );
} 