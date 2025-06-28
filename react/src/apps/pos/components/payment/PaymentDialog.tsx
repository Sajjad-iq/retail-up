import React, { useState, useCallback } from 'react';
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
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { CheckCircle, Calendar, DollarSign } from 'lucide-react';
import { useCart, useTransactions } from '../../hooks/use-pos';
import { usePaymentPlans } from '@/apps/payment-plans/hooks/use-payment-plans';
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
    const [selectedPaymentMethod, setSelectedPaymentMethod] = useState<PaymentMethod>('cash');
    const [amountReceived, setAmountReceived] = useState<number>(0);
    const [customer, setCustomer] = useState<Customer | null>(null);

    // Payment plan states
    const [downPayment, setDownPayment] = useState<number>(0);
    const [planDurationMonths, setPlanDurationMonths] = useState<number>(12);
    const [interestRate, setInterestRate] = useState<number>(0);

    const { cart, subtotal, tax, total, formattedTotal, isEmpty, clearCart } = useCart();
    const { processPayment } = useTransactions();
    const { createPlan } = usePaymentPlans();

    // Calculate change for cash payments
    const change = selectedPaymentMethod === 'cash' && amountReceived > 0
        ? Math.max(0, amountReceived - total)
        : 0;

    const isValidPayment = selectedPaymentMethod === 'cash'
        ? amountReceived >= total
        : selectedPaymentMethod === 'installment'
            ? customer && downPayment >= 0 && planDurationMonths > 0
            : true;

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (isEmpty || !isValidPayment) return;

        setIsProcessing(true);
        try {
            if (selectedPaymentMethod === 'installment') {
                // Create payment plan
                if (!customer) {
                    console.error('Customer is required for payment plans');
                    return;
                }

                const planResult = createPlan({
                    customer: {
                        id: customer.id,
                        name: customer.name,
                        email: customer.email || '',
                        phone: customer.phone || ''
                    },
                    totalAmount: total,
                    downPayment: downPayment,
                    planDurationMonths: planDurationMonths,
                    interestRate: interestRate,
                    transactionId: `TXN-${Date.now()}`
                });

                if (planResult.success) {
                    // Process down payment if any
                    if (downPayment > 0) {
                        const downPaymentTransaction: Transaction = {
                            id: `DOWN-${Date.now()}`,
                            items: cart,
                            customer: customer,
                            subtotal: downPayment,
                            discount: 0,
                            tax: 0,
                            total: downPayment,
                            payments: [{ method: 'cash', amount: downPayment }],
                            timestamp: new Date(),
                            status: 'completed',
                            cashier: 'Current User'
                        };
                        setCompletedTransaction(downPaymentTransaction);
                        onSuccess?.(downPaymentTransaction);
                    } else {
                        // Create a virtual transaction for the payment plan
                        const virtualTransaction: Transaction = {
                            id: `PLAN-${Date.now()}`,
                            items: cart,
                            customer: customer,
                            subtotal: subtotal,
                            discount: 0,
                            tax: tax,
                            total: total,
                            payments: [{ method: 'installment', amount: total }],
                            timestamp: new Date(),
                            status: 'completed',
                            cashier: 'Current User'
                        };
                        setCompletedTransaction(virtualTransaction);
                        onSuccess?.(virtualTransaction);
                    }
                } else {
                    console.error('Failed to create payment plan:', planResult.error);
                }
            } else {
                // Normal payment processing
                const result = await processPayment(selectedPaymentMethod);

                if (result.success && 'transaction' in result && result.transaction) {
                    setCompletedTransaction(result.transaction);
                    onSuccess?.(result.transaction);
                }
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
        setSelectedPaymentMethod('cash');
        setAmountReceived(0);
        setCustomer(null);
        setCompletedTransaction(null);
        setDownPayment(0);
        setPlanDurationMonths(12);
        setInterestRate(0);
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
                                <span className="capitalize">{completedTransaction.payments[0]?.method}</span>
                            </div>

                            {completedTransaction.payments[0]?.method === 'cash' && change > 0 && (
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

                            {selectedPaymentMethod === 'installment' && (
                                <Card>
                                    <CardHeader>
                                        <CardTitle className="flex items-center gap-2 text-sm">
                                            <Calendar className="h-4 w-4" />
                                            Payment Plan Details
                                        </CardTitle>
                                    </CardHeader>
                                    <CardContent className="space-y-4">
                                        <div className="grid grid-cols-2 gap-4">
                                            <div className="space-y-2">
                                                <Label htmlFor="downPayment">Down Payment</Label>
                                                <Input
                                                    id="downPayment"
                                                    type="number"
                                                    min="0"
                                                    max={total}
                                                    step="0.01"
                                                    value={downPayment || ''}
                                                    onChange={(e) => setDownPayment(Number(e.target.value) || 0)}
                                                    placeholder="0.00"
                                                />
                                            </div>
                                            <div className="space-y-2">
                                                <Label htmlFor="duration">Duration (months)</Label>
                                                <Select value={planDurationMonths.toString()} onValueChange={(value) => setPlanDurationMonths(Number(value))}>
                                                    <SelectTrigger>
                                                        <SelectValue />
                                                    </SelectTrigger>
                                                    <SelectContent>
                                                        <SelectItem value="6">6 months</SelectItem>
                                                        <SelectItem value="12">12 months</SelectItem>
                                                        <SelectItem value="18">18 months</SelectItem>
                                                        <SelectItem value="24">24 months</SelectItem>
                                                        <SelectItem value="36">36 months</SelectItem>
                                                    </SelectContent>
                                                </Select>
                                            </div>
                                        </div>
                                        <div className="space-y-2">
                                            <Label htmlFor="interestRate">Interest Rate (%)</Label>
                                            <Input
                                                id="interestRate"
                                                type="number"
                                                min="0"
                                                max="30"
                                                step="0.1"
                                                value={interestRate || ''}
                                                onChange={(e) => setInterestRate(Number(e.target.value) || 0)}
                                                placeholder="0.0"
                                            />
                                        </div>
                                        <div className="bg-muted/50 p-3 rounded-md">
                                            <div className="flex items-center gap-2 text-sm font-medium mb-2">
                                                <DollarSign className="h-4 w-4" />
                                                Plan Summary
                                            </div>
                                            <div className="space-y-1 text-sm">
                                                <div className="flex justify-between">
                                                    <span>Total Amount:</span>
                                                    <span>{formatPrice(total)}</span>
                                                </div>
                                                <div className="flex justify-between">
                                                    <span>Down Payment:</span>
                                                    <span>{formatPrice(downPayment)}</span>
                                                </div>
                                                <div className="flex justify-between">
                                                    <span>Remaining Balance:</span>
                                                    <span>{formatPrice(total - downPayment)}</span>
                                                </div>
                                                <div className="flex justify-between">
                                                    <span>Monthly Payment:</span>
                                                    <span>
                                                        {formatPrice(planDurationMonths > 0 ? (total - downPayment) / planDurationMonths : 0)}
                                                    </span>
                                                </div>
                                            </div>
                                        </div>
                                        {!customer && (
                                            <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded-md">
                                                Customer information is required for payment plans
                                            </div>
                                        )}
                                    </CardContent>
                                </Card>
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
                            {isProcessing
                                ? 'Processing...'
                                : selectedPaymentMethod === 'installment'
                                    ? downPayment > 0
                                        ? `Pay Down Payment ${formatPrice(downPayment)} & Create Plan`
                                        : 'Create Payment Plan'
                                    : `Pay ${formattedTotal}`
                            }
                        </Button>
                    </DialogFooter>
                </form>
            </DialogContent>
        </Dialog>
    );
}

export default PaymentDialog; 