import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Badge } from '@/components/ui/badge';
import {
    Store,
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
    const [searchQuery, setSearchQuery] = useState('');
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
        <div className="full grid grid-rows-[73px_1fr] bg-background">
            {/* Header */}
            <header className="border-b bg-card">
                <div className="flex items-center justify-between p-4">
                    <div className="flex items-center gap-3">
                        <div className="flex items-center gap-2">
                            <Store className="h-6 w-6 text-primary" />
                            <h1 className="text-xl font-bold">RetailUp POS</h1>
                        </div>
                        <Badge variant="secondary" className="text-xs">v1.0.0</Badge>
                    </div>

                    <div className="flex items-center gap-4">
                        {/* Today's Stats */}
                        <div className="hidden md:flex items-center gap-6">
                            <div className="text-center">
                                <p className="text-xs text-muted-foreground">Today's Sales</p>
                                <p className="text-sm font-semibold">{todaysSalesCount}</p>
                            </div>
                            <div className="text-center">
                                <p className="text-xs text-muted-foreground">Revenue</p>
                                <p className="text-sm font-semibold">{todaysRevenue}</p>
                            </div>
                        </div>

                        {/* User Avatar */}
                        <Avatar className="h-8 w-8">
                            <AvatarFallback>
                                <User className="h-4 w-4" />
                            </AvatarFallback>
                        </Avatar>
                    </div>
                </div>
            </header>

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