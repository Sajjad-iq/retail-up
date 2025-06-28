import { useState } from 'react';
import { Button } from '@/components/ui/button';
import {
    CreditCard,
    Receipt,
    User,
    Clock,
    Scan,
    Percent,
    RotateCcw,
    Pause
} from 'lucide-react';

import { ProductGrid } from '../components/products';
import { Cart } from '../components/cart';
import { PaymentDialog } from '../components/payment';
import { BarcodeScanner } from '../components/products/BarcodeScanner';
import { ReturnDialog } from '../components/returns';
import { HoldTransactionDialog } from '../components/hold/HoldTransactionDialog';
import { useCart, useTransactions, useProducts } from '../hooks/use-pos';
import { formatPrice, formatTime } from '../lib/utils/pos-utils';
import type { Transaction, Product, ReturnFormData } from '../types/pos';

/**
 * POSInterface Component
 * 
 * Main point-of-sale interface matching the Svelte design.
 * Two-column layout with products on left, cart/actions on right.
 */
export function POSInterface() {
    const [showPaymentDialog, setShowPaymentDialog] = useState(false);
    const [showBarcodeScanner, setShowBarcodeScanner] = useState(false);
    const [showReturnDialog, setShowReturnDialog] = useState(false);
    const [showHoldDialog, setShowHoldDialog] = useState(false);

    const { cart, formattedTotal, isEmpty, addToCart } = useCart();
    const { products } = useProducts();
    const {
        recentTransactions,
        todaysRevenue,
        todaysSalesCount,
        transactions
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

    const handleProductScanned = (product: Product) => {
        addToCart(product);
        setShowBarcodeScanner(false);
    };

    const handleReturn = async (returnData: ReturnFormData) => {
        console.log('Processing return:', returnData);
        // In a real implementation, this would process the return
        return {
            id: `RET-${Date.now()}`,
            originalTransactionId: returnData.originalTransactionId,
            items: [],
            reason: returnData.reason,
            refundAmount: 0,
            refundMethod: returnData.refundMethod,
            timestamp: new Date(),
            processedBy: 'Current User'
        };
    };

    const handleHoldTransaction = (note: string) => {
        console.log('Holding transaction:', note);
        // In a real implementation, this would hold the current transaction
    };

    const handleResumeTransaction = (heldTransaction: any) => {
        console.log('Resuming transaction:', heldTransaction);
        // In a real implementation, this would resume the held transaction
    };

    const handleDeleteHeldTransaction = (transactionId: string) => {
        console.log('Deleting held transaction:', transactionId);
        // In a real implementation, this would delete the held transaction
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
                <div className="w-96 border-l bg-muted/20 flex flex-col overflow-auto">
                    {/* Cart */}
                    <div className="flex-1 p-4">
                        <Cart />
                    </div>

                    {/* Checkout Actions */}
                    <div className="border-t p-4 space-y-3 overflow-auto">
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
                                onClick={() => setShowHoldDialog(true)}
                                aria-label="Hold or resume transaction"
                            >
                                <Pause className="mr-2 h-4 w-4" />
                                Hold
                            </Button>
                            <Button
                                variant="outline"
                                size="sm"
                                onClick={() => setShowBarcodeScanner(true)}
                                aria-label="Open barcode scanner"
                            >
                                <Scan className="mr-2 h-4 w-4" />
                                Scan
                            </Button>
                        </div>

                        {/* Additional Actions */}
                        <div className="grid grid-cols-1 gap-2">
                            <Button
                                variant="outline"
                                size="sm"
                                onClick={() => setShowReturnDialog(true)}
                                aria-label="Process return"
                            >
                                <RotateCcw className="mr-2 h-4 w-4" />
                                Return
                            </Button>
                        </div>

                        {/* Management Actions */}
                        <div className="grid grid-cols-1 gap-2">
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
                                                {transaction.payments?.[0]?.method || 'cash'}
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

            {/* Barcode Scanner */}
            {showBarcodeScanner && (
                <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
                    <div className="w-96">
                        <BarcodeScanner
                            onProductScanned={handleProductScanned}
                            onClose={() => setShowBarcodeScanner(false)}
                        />
                    </div>
                </div>
            )}

            {/* Return Dialog */}
            <ReturnDialog
                isOpen={showReturnDialog}
                onClose={() => setShowReturnDialog(false)}
                onReturn={handleReturn}
                transactions={transactions}
            />

            {/* Hold Transaction Dialog */}
            <HoldTransactionDialog
                isOpen={showHoldDialog}
                onClose={() => setShowHoldDialog(false)}
                cartItems={cart}
                currentCustomer={null}
                heldTransactions={[]}
                onHoldTransaction={handleHoldTransaction}
                onResumeTransaction={handleResumeTransaction}
                onDeleteHeldTransaction={handleDeleteHeldTransaction}
            />

        </div>
    );
} 