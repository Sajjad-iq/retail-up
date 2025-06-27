import { useState } from 'react';
import { Button } from '@/components/ui/button';
import {
    CreditCard,
    Receipt,
    User,
    Clock
} from 'lucide-react';

import { ProductGrid } from '../components/products';
import { Cart } from '../components/cart';
import { PaymentDialog } from '../components/payment';
import { useCart, useTransactions } from '../hooks/use-pos';
import { formatPrice, formatTime } from '../lib/utils/pos-utils';
import type { Transaction } from '../types/pos';

/**
 * POSInterface Component
 * 
 * Main point-of-sale interface matching the Svelte design.
 * Two-column layout with products on left, cart/actions on right.
 */
export function POSInterface() {
    const [showPaymentDialog, setShowPaymentDialog] = useState(false);

    const { cart, formattedTotal, isEmpty } = useCart();
    const {
        recentTransactions,
        todaysRevenue,
        todaysSalesCount
    } = useTransactions();

    const handleOpenPayment = () => {
        if (!isEmpty) {
            setShowPaymentDialog(true);
        }
    };

    const handlePaymentComplete = (transaction: Transaction) => {
        console.log('Payment completed:', transaction);
        setShowPaymentDialog(false);
    };

    return (
        <div className="h-full bg-background">
            {/* Main Content */}
            <div className="flex h-full">
                {/* Left Side - Products */}
                <div className="flex-1 flex flex-col p-4">
                    <ProductGrid />
                </div>

                {/* Right Side - Cart & Actions */}
                <div className="w-96 border-l bg-muted/20 flex flex-col">
                    {/* Cart */}
                    <div className="flex-1 p-4">
                        <Cart />
                    </div>

                    {/* Checkout Actions */}
                    <div className="border-t p-4 space-y-3">
                        <Button
                            className="w-full"
                            size="lg"
                            onClick={handleOpenPayment}
                            disabled={isEmpty}
                            aria-label={isEmpty ? 'Add items to cart to checkout' : `Checkout for ${formattedTotal}`}
                        >
                            <CreditCard className="mr-2 h-4 w-4" />
                            Checkout {!isEmpty ? formattedTotal : ''}
                        </Button>

                        {/* Quick Actions */}
                        <div className="grid grid-cols-2 gap-2">
                            <Button
                                variant="outline"
                                size="sm"
                                disabled
                                aria-label="Hold transaction (coming soon)"
                            >
                                <Receipt className="mr-2 h-4 w-4" />
                                Hold
                            </Button>
                            <Button
                                variant="outline"
                                size="sm"
                                disabled
                                aria-label="Add customer (coming soon)"
                            >
                                <User className="mr-2 h-4 w-4" />
                                Customer
                            </Button>
                        </div>
                    </div>

                    {/* Recent Transactions */}
                    {recentTransactions.length > 0 && (
                        <div className="border-t p-4">
                            <h3 className="text-sm font-semibold mb-3 flex items-center gap-2">
                                <Clock className="h-4 w-4" />
                                Recent Transactions
                            </h3>
                            <div className="space-y-2 max-h-32 overflow-y-auto">
                                {recentTransactions.slice(0, 3).map((transaction) => (
                                    <div
                                        key={transaction.id}
                                        className="flex items-center justify-between text-xs p-2 bg-background rounded border"
                                    >
                                        <div>
                                            <p className="font-medium">{transaction.id}</p>
                                            <p className="text-muted-foreground">
                                                {formatTime(transaction.timestamp)}
                                            </p>
                                        </div>
                                        <div className="text-right">
                                            <p className="font-semibold">
                                                {formatPrice(transaction.total)}
                                            </p>
                                            <p className="text-muted-foreground capitalize">
                                                {transaction.paymentMethod}
                                            </p>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    )}
                </div>
            </div>

            {/* Payment Dialog */}
            <PaymentDialog
                isOpen={showPaymentDialog}
                onClose={() => setShowPaymentDialog(false)}
                onSuccess={handlePaymentComplete}
            />
        </div>
    );
} 