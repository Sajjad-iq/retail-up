import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { Button } from '@/components/ui/button';
import {
    Dialog,
    DialogContent,
    DialogHeader,
    DialogTitle,
    DialogDescription,
} from '@/components/ui/dialog';
import {
    Form,
    FormControl,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from '@/components/ui/form';
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Separator } from '@/components/ui/separator';
import {
    CreditCard,
    DollarSign,
    Smartphone,
    CheckCircle,
    Loader2,
    Receipt,
} from 'lucide-react';
import { useCart, useTransactions } from '../hooks/use-pos';
import { formatPrice } from '../lib/utils/pos-utils';
import { paymentFormSchema } from '../lib/validations/pos-schemas';
import type { PaymentFormInput } from '../lib/validations/pos-schemas';
import type { PaymentMethod, Transaction } from '../types/pos';

interface PaymentDialogProps {
    /** Whether the dialog is open */
    open: boolean;
    /** Function to call when dialog should close */
    onOpenChange: (open: boolean) => void;
    /** Function to call when payment is completed */
    onPaymentComplete?: (transaction: Transaction) => void;
}

/**
 * Payment method configuration
 */
const paymentMethods = [
    { value: 'cash' as const, label: 'Cash', icon: DollarSign },
    { value: 'card' as const, label: 'Credit/Debit Card', icon: CreditCard },
    { value: 'mobile' as const, label: 'Mobile Payment', icon: Smartphone },
];

/**
 * PaymentDialog Component
 * 
 * Modal dialog for processing payments with form validation.
 * Supports multiple payment methods and shows payment confirmation.
 * 
 * @param props - Component props
 * @returns PaymentDialog component
 */
export function PaymentDialog({
    open,
    onOpenChange,
    onPaymentComplete
}: PaymentDialogProps) {
    const { cart, formattedSubtotal, formattedTax, formattedTotal, total, isEmpty } = useCart();
    const { processPayment } = useTransactions();

    const [isProcessing, setIsProcessing] = useState(false);
    const [paymentComplete, setPaymentComplete] = useState(false);
    const [completedTransaction, setCompletedTransaction] = useState<Transaction | null>(null);

    const form = useForm<PaymentFormInput>({
        resolver: zodResolver(paymentFormSchema),
        defaultValues: {
            paymentMethod: 'cash',
            amountReceived: undefined,
        },
    });

    const selectedPaymentMethod = form.watch('paymentMethod');

    /**
     * Handle payment form submission
     */
    const onSubmit = async (data: PaymentFormInput) => {
        if (isEmpty) return;

        setIsProcessing(true);

        try {
            // Simulate payment processing delay
            await new Promise(resolve => setTimeout(resolve, 2000));

            const result = await processPayment(data.paymentMethod);

            if (result.success && result.transaction) {
                setCompletedTransaction(result.transaction);
                setPaymentComplete(true);
                onPaymentComplete?.(result.transaction);
            } else {
                throw new Error(result.error || 'Payment failed');
            }
        } catch (error) {
            console.error('Payment failed:', error);
            // In a real app, you'd show an error toast or message
            alert('Payment failed. Please try again.');
        } finally {
            setIsProcessing(false);
        }
    };

    /**
 * Handle dialog close
 */
    const handleClose = () => {
        onOpenChange(false);
        setTimeout(() => {
            setPaymentComplete(false);
            setCompletedTransaction(null);
            form.reset();
        }, 300);
    };

    /**
     * Handle receipt printing
     */
    const handlePrintReceipt = () => {
        // In a real app, this would interface with a receipt printer
        alert('Receipt sent to printer!');
    };

    /**
     * Get payment method icon
     */
    const getPaymentMethodIcon = (method: PaymentMethod) => {
        const config = paymentMethods.find(pm => pm.value === method);
        return config?.icon || DollarSign;
    };

    return (
        <Dialog open={open} onOpenChange={onOpenChange}>
            <DialogContent className="sm:max-w-md">
                {!paymentComplete ? (
                    <>
                        <DialogHeader>
                            <DialogTitle>Process Payment</DialogTitle>
                            <DialogDescription>
                                Complete the transaction for {cart.length} item{cart.length !== 1 ? 's' : ''}
                            </DialogDescription>
                        </DialogHeader>

                        <div className="space-y-6">
                            {/* Order Summary */}
                            <div className="space-y-2">
                                <h4 className="font-semibold">Order Summary</h4>
                                <div className="space-y-1 text-sm">
                                    <div className="flex justify-between">
                                        <span>Subtotal:</span>
                                        <span>{formattedSubtotal}</span>
                                    </div>
                                    <div className="flex justify-between">
                                        <span>Tax (8%):</span>
                                        <span>{formattedTax}</span>
                                    </div>
                                    <Separator />
                                    <div className="flex justify-between font-bold text-lg">
                                        <span>Total:</span>
                                        <span>{formattedTotal}</span>
                                    </div>
                                </div>
                            </div>

                            {/* Payment Form */}
                            <Form {...form}>
                                <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
                                    {/* Payment Method Selection */}
                                    <FormField
                                        control={form.control}
                                        name="paymentMethod"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Payment Method</FormLabel>
                                                <Select onValueChange={field.onChange} defaultValue={field.value}>
                                                    <FormControl>
                                                        <SelectTrigger>
                                                            <SelectValue placeholder="Select payment method" />
                                                        </SelectTrigger>
                                                    </FormControl>
                                                    <SelectContent>
                                                        {paymentMethods.map((method) => {
                                                            const Icon = method.icon;
                                                            return (
                                                                <SelectItem key={method.value} value={method.value}>
                                                                    <div className="flex items-center gap-2">
                                                                        <Icon className="h-4 w-4" />
                                                                        {method.label}
                                                                    </div>
                                                                </SelectItem>
                                                            );
                                                        })}
                                                    </SelectContent>
                                                </Select>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />

                                    {/* Amount Received (for cash payments) */}
                                    {selectedPaymentMethod === 'cash' && (
                                        <FormField
                                            control={form.control}
                                            name="amountReceived"
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Amount Received</FormLabel>
                                                    <FormControl>
                                                        <Input
                                                            type="number"
                                                            step="0.01"
                                                            min={total}
                                                            placeholder={`Minimum: ${formatPrice(total)}`}
                                                            {...field}
                                                            onChange={(e) => field.onChange(parseFloat(e.target.value) || 0)}
                                                        />
                                                    </FormControl>
                                                    <FormMessage />
                                                </FormItem>
                                            )}
                                        />
                                    )}

                                    {/* Action Buttons */}
                                    <div className="flex gap-3 pt-4">
                                        <Button
                                            type="button"
                                            variant="outline"
                                            className="flex-1"
                                            onClick={handleClose}
                                            disabled={isProcessing}
                                        >
                                            Cancel
                                        </Button>
                                        <Button
                                            type="submit"
                                            className="flex-1"
                                            disabled={isProcessing}
                                        >
                                            {isProcessing ? (
                                                <>
                                                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                                    Processing...
                                                </>
                                            ) : (
                                                `Pay ${formattedTotal}`
                                            )}
                                        </Button>
                                    </div>
                                </form>
                            </Form>
                        </div>
                    </>
                ) : (
                    <>
                        <DialogHeader>
                            <DialogTitle className="flex items-center gap-2 text-green-600">
                                <CheckCircle className="h-5 w-5" />
                                Payment Successful
                            </DialogTitle>
                            <DialogDescription>
                                Transaction completed successfully
                            </DialogDescription>
                        </DialogHeader>

                        {completedTransaction && (
                            <div className="space-y-4">
                                <div className="text-center p-6 bg-green-50 rounded-lg border border-green-200">
                                    <CheckCircle className="h-12 w-12 text-green-600 mx-auto mb-4" />
                                    <h3 className="text-lg font-semibold text-green-800">Payment Complete!</h3>
                                    <p className="text-green-700">Transaction ID: {completedTransaction.id}</p>
                                    <p className="text-2xl font-bold text-green-800 mt-2">
                                        {formatPrice(completedTransaction.total)}
                                    </p>
                                </div>

                                <div className="space-y-2 text-sm">
                                    <div className="flex justify-between">
                                        <span>Payment Method:</span>
                                        <span className="capitalize">{completedTransaction.paymentMethod}</span>
                                    </div>
                                    <div className="flex justify-between">
                                        <span>Items:</span>
                                        <span>{completedTransaction.items.length}</span>
                                    </div>
                                    <div className="flex justify-between">
                                        <span>Date:</span>
                                        <span>{completedTransaction.timestamp.toLocaleString()}</span>
                                    </div>
                                </div>

                                <div className="flex gap-3">
                                    <Button
                                        variant="outline"
                                        className="flex-1"
                                        onClick={() => alert('Receipt sent to printer!')}
                                    >
                                        <Receipt className="mr-2 h-4 w-4" />
                                        Print Receipt
                                    </Button>
                                    <Button className="flex-1" onClick={handleClose}>
                                        Done
                                    </Button>
                                </div>
                            </div>
                        )}
                    </>
                )}
            </DialogContent>
        </Dialog>
    );
}

export default PaymentDialog; 