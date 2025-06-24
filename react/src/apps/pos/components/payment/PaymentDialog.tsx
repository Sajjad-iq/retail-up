import React, { useState } from 'react';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { CheckCircle } from 'lucide-react';
import { useCart, useTransactions } from '../../hooks/use-pos';
import { formatPrice, formatTime } from '../../lib/utils/pos-utils';
import {
    PaymentMethodSelector,
    CustomerForm,
    CashPaymentForm,
    OrderSummary
} from './';
import type { PaymentMethod, Transaction, Customer } from '../../types/pos';

interface PaymentDialogProps {
    isOpen: boolean;
    onClose: () => void;
    onSuccess?: (transaction: Transaction) => void;
}

/**
 * PaymentDialog Component
 * 
 * Handles payment processing with support for multiple payment methods.
 * Uses separated components for modular UI organization.
 */
export function PaymentDialog({ isOpen, onClose, onSuccess }: PaymentDialogProps) {
    const [isProcessing, setIsProcessing] = useState(false);
    const [completedTransaction, setCompletedTransaction] = useState<Transaction | null>(null);
    const [selectedPaymentMethod, setSelectedPaymentMethod] = useState<PaymentMethod>('card');
    const [amountReceived, setAmountReceived] = useState<number>(0);
    const [customer, setCustomer] = useState<Customer | null>(null);

    const { cart, subtotal, tax, total, formattedTotal, isEmpty, clearCart } = useCart();
    const { processPayment } = useTransactions();

    // Calculate change for cash payments
    const change = selectedPaymentMethod === 'cash' && amountReceived > 0
        ? Math.max(0, amountReceived - total)
        : 0;

    const isValidPayment = selectedPaymentMethod !== 'cash' || amountReceived >= total;

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (isEmpty || !isValidPayment) return;

        setIsProcessing(true);
        try {
            const result = await processPayment(selectedPaymentMethod);

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
            resetForm();
            onClose();
        }
    };

    const resetForm = () => {
        setSelectedPaymentMethod('card');
        setAmountReceived(0);
        setCustomer(null);
        setCompletedTransaction(null);
    };

    const handleNewTransaction = () => {
        resetForm();
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

                            {completedTransaction.paymentMethod === 'cash' && change > 0 && (
                                <div className="flex justify-between text-sm">
                                    <span>Change:</span>
                                    <span className="font-medium">{formatPrice(change)}</span>
                                </div>
                            )}

                            <div className="flex justify-between text-sm">
                                <span>Time:</span>
                                <span>{formatTime(completedTransaction.timestamp)}</span>
                            </div>

                            {completedTransaction.customer && (
                                <div className="flex justify-between text-sm">
                                    <span>Customer:</span>
                                    <span>{completedTransaction.customer.name}</span>
                                </div>
                            )}
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
            <DialogContent className="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle>Process Payment</DialogTitle>
                    <DialogDescription>
                        Complete the transaction by selecting a payment method
                    </DialogDescription>
                </DialogHeader>

                <form onSubmit={handleSubmit} className="space-y-6">
                    <div className="grid md:grid-cols-2 gap-6">
                        {/* Left Column - Order Details */}
                        <div className="space-y-6">
                            <OrderSummary
                                items={cart}
                                subtotal={subtotal}
                                tax={tax}
                                total={total}
                            />

                            <CustomerForm
                                customer={customer}
                                onCustomerChange={setCustomer}
                            />
                        </div>

                        {/* Right Column - Payment Details */}
                        <div className="space-y-6">
                            <PaymentMethodSelector
                                selectedMethod={selectedPaymentMethod}
                                onMethodChange={setSelectedPaymentMethod}
                            />

                            {selectedPaymentMethod === 'cash' && (
                                <CashPaymentForm
                                    total={total}
                                    amountReceived={amountReceived}
                                    onAmountReceivedChange={setAmountReceived}
                                />
                            )}

                            {selectedPaymentMethod !== 'cash' && (
                                <div className="space-y-4">
                                    <h4 className="font-medium text-sm">Payment Processing</h4>
                                    <div className="p-4 bg-muted/50 rounded-md text-center text-sm text-muted-foreground">
                                        {selectedPaymentMethod === 'card' && 'Insert or swipe card when ready'}
                                        {selectedPaymentMethod === 'mobile' && 'Present device to terminal'}
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>

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
                            disabled={isProcessing || isEmpty || !isValidPayment}
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

export default PaymentDialog; 