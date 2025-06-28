import React from 'react';
import { Card, CardContent } from '@/components/ui/card';
import { Banknote, Calendar } from 'lucide-react';
import type { PaymentMethod } from '../../types/pos';

interface PaymentMethodSelectorProps {
    selectedMethod: PaymentMethod;
    onMethodChange: (method: PaymentMethod) => void;
}

/**
 * PaymentMethodSelector Component
 * 
 * Displays payment method options in a grid layout with icons.
 * Allows users to select between cash and installment payment plans.
 */
export function PaymentMethodSelector({
    selectedMethod,
    onMethodChange
}: PaymentMethodSelectorProps) {
    const paymentMethods: Array<{
        method: PaymentMethod;
        label: string;
        icon: React.ComponentType<any>
    }> = [
            { method: 'cash', label: 'Cash', icon: Banknote },
            { method: 'installment', label: 'Payment Plan', icon: Calendar },
        ];

    return (
        <div className="space-y-4">
            <h4 className="font-medium text-sm">Payment Method</h4>
            <div className="grid grid-cols-2 gap-3">
                {paymentMethods.map(({ method, label, icon: Icon }) => (
                    <Card
                        key={method}
                        className={`cursor-pointer transition-colors ${selectedMethod === method
                            ? 'border-primary bg-primary/5'
                            : 'hover:border-muted-foreground/30'
                            }`}
                        onClick={() => onMethodChange(method)}
                    >
                        <CardContent className="flex flex-col items-center justify-center p-4">
                            <Icon className="h-6 w-6 mb-2" />
                            <span className="text-sm font-medium">{label}</span>
                        </CardContent>
                    </Card>
                ))}
            </div>
        </div>
    );
}