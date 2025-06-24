import React from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { TableCell, TableRow } from '@/components/ui/table';
import { Trash2, Plus, Minus } from 'lucide-react';
import { formatPrice } from '../../lib/utils/pos-utils';
import type { CartItem } from '../../types/pos';

interface CartItemRowProps {
    item: CartItem;
    onQuantityChange: (productId: string, value: string) => void;
    onIncrement: (productId: string, currentQuantity: number) => void;
    onDecrement: (productId: string, currentQuantity: number) => void;
    onRemove: (productId: string) => void;
}

/**
 * CartItemRow Component
 * 
 * Displays a single cart item in a table row with quantity controls and remove button.
 * Includes product information, quantity adjustment controls, price calculation, and remove action.
 */
export function CartItemRow({
    item,
    onQuantityChange,
    onIncrement,
    onDecrement,
    onRemove,
}: CartItemRowProps) {
    return (
        <TableRow>
            <TableCell className="font-medium">
                <div>
                    <p className="font-semibold">{item.product.name}</p>
                    <p className="text-sm text-muted-foreground">
                        {formatPrice(item.product.price)} each
                    </p>
                </div>
            </TableCell>

            <TableCell className="text-center">
                <div className="flex items-center justify-center gap-2">
                    <Button
                        variant="outline"
                        size="sm"
                        className="h-6 w-6 p-0"
                        onClick={() => onDecrement(item.product.id, item.quantity)}
                        aria-label="Decrease quantity"
                    >
                        <Minus className="h-3 w-3" />
                    </Button>

                    <Input
                        type="number"
                        min="1"
                        value={item.quantity}
                        onChange={(e) => onQuantityChange(item.product.id, e.target.value)}
                        className="w-16 h-6 text-center text-xs"
                        aria-label={`Quantity for ${item.product.name}`}
                    />

                    <Button
                        variant="outline"
                        size="sm"
                        className="h-6 w-6 p-0"
                        onClick={() => onIncrement(item.product.id, item.quantity)}
                        aria-label="Increase quantity"
                    >
                        <Plus className="h-3 w-3" />
                    </Button>
                </div>
            </TableCell>

            <TableCell className="text-right font-semibold">
                {formatPrice(item.product.price * item.quantity)}
            </TableCell>

            <TableCell>
                <Button
                    variant="ghost"
                    size="sm"
                    className="h-6 w-6 p-0 text-destructive hover:text-destructive"
                    onClick={() => onRemove(item.product.id)}
                    aria-label={`Remove ${item.product.name} from cart`}
                >
                    <Trash2 className="h-3 w-3" />
                </Button>
            </TableCell>
        </TableRow>
    );
} 