import React from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { formatPrice } from '../../lib/utils/pos-utils';

interface CashPaymentFormProps {
    total: number;
    amountReceived: number;
    onAmountReceivedChange: (amount: number) => void;
}

/**
 * CashPaymentForm Component
 * 
 * Handles cash payment input with quick amount buttons and change calculation.
 * Provides exact amount button and common bill denomination shortcuts.
 */
export function CashPaymentForm({
    total,
    amountReceived,
    onAmountReceivedChange
}: CashPaymentFormProps) {
    const change = amountReceived - total;
    const isValidPayment = amountReceived >= total;

    const quickAmounts = [
        { label: 'Exact', amount: total },
        { label: '$5', amount: 5 },
        { label: '$10', amount: 10 },
        { label: '$20', amount: 20 },
        { label: '$50', amount: 50 },
        { label: '$100', amount: 100 },
    ].filter(item => item.amount >= total || item.label === 'Exact');

    const handleQuickAmount = (amount: number) => {
        onAmountReceivedChange(amount);
    };

    return (
        <div className="space-y-4">
            <h4 className="font-medium text-sm">Cash Payment</h4>

            <div>
                <Label htmlFor="amount-received" className="text-xs">Amount Received</Label>
                <Input
                    id="amount-received"
                    type="number"
                    step="0.01"
                    min={total}
                    value={amountReceived || ''}
                    onChange={(e) => onAmountReceivedChange(parseFloat(e.target.value) || 0)}
                    placeholder={formatPrice(total)}
                    className="h-8"
                />
            </div>

            <div className="grid grid-cols-3 gap-2">
                {quickAmounts.map(({ label, amount }) => (
                    <Button
                        key={label}
                        variant="outline"
                        size="sm"
                        onClick={() => handleQuickAmount(amount)}
                        className="text-xs"
                    >
                        {label}
                    </Button>
                ))}
            </div>

            {amountReceived > 0 && (
                <div className="space-y-2 p-3 bg-muted/50 rounded-md">
                    <div className="flex justify-between text-sm">
                        <span>Total:</span>
                        <span>{formatPrice(total)}</span>
                    </div>
                    <div className="flex justify-between text-sm">
                        <span>Received:</span>
                        <span>{formatPrice(amountReceived)}</span>
                    </div>
                    <div className={`flex justify-between font-medium ${change >= 0 ? 'text-green-600' : 'text-red-600'
                        }`}>
                        <span>Change:</span>
                        <span>{formatPrice(Math.max(0, change))}</span>
                    </div>
                    {!isValidPayment && (
                        <p className="text-xs text-red-600">
                            Insufficient amount received
                        </p>
                    )}
                </div>
            )}
        </div>
    );
} 