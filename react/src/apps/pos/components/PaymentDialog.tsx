import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from '@/components/ui/select';
import { Separator } from '@/components/ui/separator';
import { Badge } from '@/components/ui/badge';
import {
    CreditCard,
    Smartphone,
    DollarSign,
    CheckCircle,
} from 'lucide-react';
import { useCart, useTransactions } from '../hooks/use-pos';
import { formatPrice } from '../lib/utils/pos-utils';
import { paymentFormSchema, type PaymentFormInput } from '../lib/validations/pos-schemas';
import type { PaymentMethod, Transaction } from '../types/pos';

interface PaymentDialogProps {
    isOpen: boolean;
    onClose: () => void;
    onSuccess?: (transaction: Transaction) => void;
}

/**
 * PaymentDialog Component
 * 
 * Handles payment processing with support for multiple payment methods.
 * Includes form validation and success confirmation.
 */
export function PaymentDialog({ isOpen, onClose, onSuccess }: PaymentDialogProps) {
    const [isProcessing, setIsProcessing] = useState(false);
    const [completedTransaction, setCompletedTransaction] = useState<Transaction | null>(null);

    const { formattedSubtotal, formattedTax, formattedTotal, total, isEmpty } = useCart();
    const { processPayment } = useTransactions();

    const form = useForm<PaymentFormInput>({
        resolver: zodResolver(paymentFormSchema),
        defaultValues: {
            paymentMethod: 'card',
        },
    });

    const selectedPaymentMethod = form.watch('paymentMethod');
    const amountReceived = form.watch('amountReceived');

    // Calculate change for cash payments
    const change = selectedPaymentMethod === 'cash' && amountReceived
        ? Math.max(0, amountReceived - total)
        : 0;

    const paymentMethods = [
        {
            value: 'cash' as PaymentMethod,
            label: 'Cash',
            icon: DollarSign,
            description: 'Physical cash payment',
        },
        {
            value: 'card' as PaymentMethod,
            label: 'Card',
            icon: CreditCard,
            description: 'Credit or debit card',
        },
        {
            value: 'mobile' as PaymentMethod,
            label: 'Mobile Pay',
            icon: Smartphone,
            description: 'Apple Pay, Google Pay, etc.',
        },
    ];

    const selectedMethod = paymentMethods.find(method => method.value === selectedPaymentMethod);

    const onSubmit = async (data: PaymentFormInput) => {
        if (isEmpty) return;

        setIsProcessing(true);
        try {
            const result = await processPayment(data.paymentMethod);

            if (result.success && result.transaction) {
                setCompletedTransaction(result.transaction);
                onSuccess?.(result.transaction);
            }
        } catch (error) {
            console.error('Payment processing failed:', error);
        } finally {
            setIsProcessing(false);
        }
    };

    const handleClose = () => {
        if (!isProcessing) {
            form.reset();
            setCompletedTransaction(null);
            onClose();
        }
    };

    const handleNewTransaction = () => {
        form.reset();
        setCompletedTransaction(null);
        onClose();
    };

    // Success screen
    if (completedTransaction) {
        return (
            <Dialog open={isOpen} onOpenChange={handleClose}>
                <DialogContent className="sm:max-w-md">
                    <DialogHeader>
                        <DialogTitle className="flex items-center gap-2 text-green-600">
                            <CheckCircle className="h-5 w-5" />
                            Payment Successful
                        </DialogTitle>
                        <DialogDescription>
                            Transaction completed successfully
                        </DialogDescription>
                    </DialogHeader>

                    <div className="space-y-4">
                        <div className="text-center">
                            <p className="text-2xl font-bold text-green-600">
                                {formatPrice(completedTransaction.total)}
                            </p>
                            <p className="text-sm text-muted-foreground">
                                Transaction ID: {completedTransaction.id}
                            </p>
                        </div>

                        <div className="bg-muted p-4 rounded-lg space-y-2">
                            <div className="flex justify-between text-sm">
                                <span>Payment Method:</span>
                                <span className="capitalize">{completedTransaction.paymentMethod}</span>
                            </div>

                            {selectedPaymentMethod === 'cash' && change > 0 && (
                                <div className="flex justify-between text-sm">
                                    <span>Change:</span>
                                    <span className="font-medium">{formatPrice(change)}</span>
                                </div>
                            )}

                            <div className="flex justify-between text-sm">
                                <span>Time:</span>
                                <span>{new Date(completedTransaction.timestamp).toLocaleTimeString()}</span>
                            </div>
                        </div>
                    </div>

                    <DialogFooter>
                        <Button onClick={handleNewTransaction} className="w-full">
                            New Transaction
                        </Button>
                    </DialogFooter>
                </DialogContent>
            </Dialog>
        );
    }

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="sm:max-w-md">
                <DialogHeader>
                    <DialogTitle>Process Payment</DialogTitle>
                    <DialogDescription>
                        Complete the transaction by selecting a payment method
                    </DialogDescription>
                </DialogHeader>

                <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
                    {/* Order Summary */}
                    <div className="bg-muted p-4 rounded-lg space-y-2">
                        <div className="flex justify-between text-sm">
                            <span>Subtotal:</span>
                            <span>{formattedSubtotal}</span>
                        </div>
                        <div className="flex justify-between text-sm">
                            <span>Tax:</span>
                            <span>{formattedTax}</span>
                        </div>
                        <Separator />
                        <div className="flex justify-between font-semibold">
                            <span>Total:</span>
                            <span>{formattedTotal}</span>
                        </div>
                    </div>

                    {/* Payment Method Selection */}
                    <div className="space-y-3">
                        <Label htmlFor="paymentMethod">Payment Method</Label>
                        <Select
                            value={selectedPaymentMethod}
                            onValueChange={(value: PaymentMethod) =>
                                form.setValue('paymentMethod', value)
                            }
                        >
                            <SelectTrigger>
                                <SelectValue />
                            </SelectTrigger>
                            <SelectContent>
                                {paymentMethods.map((method) => {
                                    const Icon = method.icon;
                                    return (
                                        <SelectItem key={method.value} value={method.value}>
                                            <div className="flex items-center gap-2">
                                                <Icon className="h-4 w-4" />
                                                <span>{method.label}</span>
                                            </div>
                                        </SelectItem>
                                    );
                                })}
                            </SelectContent>
                        </Select>

                        {selectedMethod && (
                            <div className="flex items-center gap-2 text-sm text-muted-foreground">
                                <selectedMethod.icon className="h-4 w-4" />
                                <span>{selectedMethod.description}</span>
                            </div>
                        )}
                    </div>

                    {/* Cash Payment Fields */}
                    {selectedPaymentMethod === 'cash' && (
                        <div className="space-y-3">
                            <Label htmlFor="amountReceived">Amount Received</Label>
                            <Input
                                {...form.register('amountReceived', { valueAsNumber: true })}
                                type="number"
                                step="0.01"
                                min={total}
                                placeholder={`Minimum: ${formatPrice(total)}`}
                            />

                            {form.formState.errors.amountReceived && (
                                <p className="text-sm text-destructive">
                                    {form.formState.errors.amountReceived.message}
                                </p>
                            )}

                            {change > 0 && (
                                <div className="flex items-center justify-between p-3 bg-green-50 border border-green-200 rounded-lg">
                                    <span className="text-sm text-green-700">Change:</span>
                                    <Badge variant="outline" className="text-green-700 border-green-300">
                                        {formatPrice(change)}
                                    </Badge>
                                </div>
                            )}
                        </div>
                    )}

                    <DialogFooter className="gap-2">
                        <Button
                            type="button"
                            variant="outline"
                            onClick={handleClose}
                            disabled={isProcessing}
                        >
                            Cancel
                        </Button>
                        <Button
                            type="submit"
                            disabled={isProcessing || isEmpty}
                            className="flex-1"
                        >
                            {isProcessing ? 'Processing...' : `Pay ${formattedTotal}`}
                        </Button>
                    </DialogFooter>
                </form>
            </DialogContent>
        </Dialog>
    );
} 