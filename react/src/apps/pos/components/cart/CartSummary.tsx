import React from 'react';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';

interface CartSummaryProps {
    subtotal: string;
    tax: string;
    total: string;
    onCheckout?: () => void;
    isEmpty: boolean;
}

/**
 * CartSummary Component
 * 
 * Displays the cart pricing breakdown including subtotal, tax, and total.
 * Optionally includes a checkout button when onCheckout is provided.
 */
export function CartSummary({
    subtotal,
    tax,
    total,
    onCheckout,
    isEmpty
}: CartSummaryProps) {
    return (
        <div className="border-t p-6">
            <div className="space-y-4">
                <div className="space-y-2">
                    <div className="flex justify-between text-sm">
                        <span>Subtotal:</span>
                        <span>{subtotal}</span>
                    </div>
                    <div className="flex justify-between text-sm">
                        <span>Tax (8%):</span>
                        <span>{tax}</span>
                    </div>
                    <Separator />
                    <div className="flex justify-between text-lg font-bold">
                        <span>Total:</span>
                        <span>{total}</span>
                    </div>
                </div>

                {onCheckout && (
                    <Button
                        className="w-full"
                        size="lg"
                        onClick={onCheckout}
                        disabled={isEmpty}
                    >
                        Proceed to Payment
                    </Button>
                )}
            </div>
        </div>
    );
} 