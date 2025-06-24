import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Badge } from '@/components/ui/badge';
import {
    Store,
    Search,
    CreditCard,
    Receipt,
    User,
    Clock,
    DollarSign,
} from 'lucide-react';
import { ProductGrid } from './ProductGrid';
import { Cart } from './Cart';
import { PaymentDialog } from './PaymentDialog';
import { useCart, useTransactions, usePOSFormatters } from '../hooks/use-pos';
import type { Transaction } from '../types/pos';

/**
 * POSInterface Component
 * 
 * Main point of sale interface that combines product selection, cart management,
 * and payment processing in a unified dashboard.
 * 
 * @returns POSInterface component
 */
export function POSInterface() {
    const [searchQuery, setSearchQuery] = useState('');
    const [showPaymentDialog, setShowPaymentDialog] = useState(false);

    const { cart, formattedTotal, total, isEmpty } = useCart();
    const {
        todaysTransactions,
        recentTransactions,
        todaysRevenue,
        todaysSalesCount
    } = useTransactions();
    const { formatTime } = usePOSFormatters();

    /**
     * Handle opening payment dialog
     */
    const handleOpenPayment = () => {
        if (!isEmpty) {
            setShowPaymentDialog(true);
        }
    };

    /**
     * Handle payment completion
     */
    const handlePaymentComplete = (transaction: Transaction) => {
        console.log('Payment completed:', transaction);
        setShowPaymentDialog(false);
    };

    return (
        <div className="h-screen bg-background">
            {/* Header */}
            <POSHeader
                todaysSalesCount={todaysSalesCount}
                todaysRevenue={todaysRevenue}
            />

            {/* Main Content */}
            <div className="flex h-[calc(100vh-73px)]">
                {/* Left Side - Products */}
                <div className="flex-1 flex flex-col">
                    {/* Search Bar */}
                    <div className="p-4 border-b">
                        <div className="relative">
                            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                            <Input
                                placeholder="Search products..."
                                value={searchQuery}
                                onChange={(e) => setSearchQuery(e.target.value)}
                                className="pl-10"
                                aria-label="Search products"
                            />
                        </div>
                    </div>

                    {/* Product Grid */}
                    <div className="flex-1 p-4">
                        <ProductGrid searchQuery={searchQuery} />
                    </div>
                </div>

                {/* Right Side - Cart & Actions */}
                <div className="w-96 border-l bg-muted/20 flex flex-col">
                    {/* Cart */}
                    <div className="flex-1 p-4">
                        <Cart />
                    </div>

                    {/* Checkout Actions */}
                    <CheckoutActions
                        isEmpty={isEmpty}
                        formattedTotal={formattedTotal}
                        onOpenPayment={handleOpenPayment}
                    />

                    {/* Recent Transactions */}
                    {recentTransactions.length > 0 && (
                        <RecentTransactions
                            transactions={recentTransactions}
                            formatTime={formatTime}
                        />
                    )}
                </div>
            </div>

            {/* Payment Dialog */}
            <PaymentDialog
                open={showPaymentDialog}
                onOpenChange={setShowPaymentDialog}
                onPaymentComplete={handlePaymentComplete}
            />
        </div>
    );
}

/**
 * POS Header Component
 */
interface POSHeaderProps {
    todaysSalesCount: number;
    todaysRevenue: string;
}

function POSHeader({ todaysSalesCount, todaysRevenue }: POSHeaderProps) {
    return (
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
    );
}

/**
 * Checkout Actions Component
 */
interface CheckoutActionsProps {
    isEmpty: boolean;
    formattedTotal: string;
    onOpenPayment: () => void;
}

function CheckoutActions({ isEmpty, formattedTotal, onOpenPayment }: CheckoutActionsProps) {
    return (
        <div className="border-t p-4 space-y-3">
            <Button
                className="w-full"
                size="lg"
                onClick={onOpenPayment}
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
    );
}

/**
 * Recent Transactions Component
 */
interface RecentTransactionsProps {
    transactions: Transaction[];
    formatTime: (date: Date) => string;
}

function RecentTransactions({ transactions, formatTime }: RecentTransactionsProps) {
    return (
        <div className="border-t p-4">
            <h3 className="text-sm font-semibold mb-3 flex items-center gap-2">
                <Clock className="h-4 w-4" />
                Recent Transactions
            </h3>
            <div className="space-y-2 max-h-32 overflow-y-auto">
                {transactions.map((transaction) => (
                    <Card key={transaction.id} className="p-2">
                        <div className="flex items-center justify-between text-xs">
                            <div>
                                <p className="font-medium">{transaction.id}</p>
                                <p className="text-muted-foreground">
                                    {formatTime(transaction.timestamp)}
                                </p>
                            </div>
                            <div className="text-right">
                                <p className="font-semibold">
                                    ${transaction.total.toFixed(2)}
                                </p>
                                <p className="text-muted-foreground capitalize">
                                    {transaction.paymentMethod}
                                </p>
                            </div>
                        </div>
                    </Card>
                ))}
            </div>
        </div>
    );
}

export default POSInterface; 