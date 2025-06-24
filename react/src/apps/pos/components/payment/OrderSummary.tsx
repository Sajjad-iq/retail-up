import React from 'react';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Separator } from '@/components/ui/separator';
import { formatPrice } from '../../lib/utils/pos-utils';
import type { CartItem } from '../../types/pos';

interface OrderSummaryProps {
    items: CartItem[];
    subtotal: number;
    tax: number;
    total: number;
}

/**
 * OrderSummary Component
 * 
 * Displays the complete order summary including items, quantities, prices, and totals.
 * Shows a detailed breakdown suitable for payment confirmation.
 */
export function OrderSummary({ items, subtotal, tax, total }: OrderSummaryProps) {
    return (
        <div className="space-y-4">
            <h4 className="font-medium text-sm">Order Summary</h4>

            <ScrollArea className="max-h-64">
                <div className="space-y-3">
                    {items.map((item) => (
                        <div key={item.product.id} className="flex justify-between text-sm">
                            <div className="flex-1">
                                <p className="font-medium">{item.product.name}</p>
                                <p className="text-muted-foreground">
                                    {formatPrice(item.product.price)} × {item.quantity}
                                </p>
                            </div>
                            <div className="text-right">
                                <p className="font-medium">
                                    {formatPrice(item.product.price * item.quantity)}
                                </p>
                            </div>
                        </div>
                    ))}
                </div>
            </ScrollArea>

            <div className="space-y-2">
                <Separator />
                <div className="flex justify-between text-sm">
                    <span>Subtotal:</span>
                    <span>{formatPrice(subtotal)}</span>
                </div>
                <div className="flex justify-between text-sm">
                    <span>Tax (8%):</span>
                    <span>{formatPrice(tax)}</span>
                </div>
                <Separator />
                <div className="flex justify-between text-lg font-bold">
                    <span>Total:</span>
                    <span>{formatPrice(total)}</span>
                </div>
            </div>
        </div>
    );
} 