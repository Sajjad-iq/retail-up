import React from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { ScrollArea } from '@/components/ui/scroll-area';
import {
    Table,
    TableBody,
    TableHead,
    TableHeader,
    TableRow,
} from '@/components/ui/table';
import { useCart } from '../../hooks/use-pos';
import { EmptyCartMessage, CartItemRow, CartSummary } from './';

interface CartProps {
    /** CSS class name for styling */
    className?: string;
    /** Function to call when checkout is initiated */
    onCheckout?: () => void;
}

/**
 * Cart Component
 * 
 * Displays current cart items with quantity controls and pricing breakdown.
 * Allows users to modify quantities, remove items, and clear the entire cart.
 * 
 * @param props - Component props
 * @returns Cart component
 */
export function Cart({ className, onCheckout }: CartProps) {
    const {
        cart,
        subtotal,
        tax,
        total,
        formattedSubtotal,
        formattedTax,
        formattedTotal,
        isEmpty,
        updateCartQuantity,
        removeFromCart,
        clearCart,
    } = useCart();

    /**
     * Handle quantity input change
     */
    const handleQuantityChange = (productId: string, value: string) => {
        const quantity = parseInt(value) || 1;
        if (quantity > 0) {
            updateCartQuantity(productId, quantity);
        }
    };

    /**
     * Handle quantity increment
     */
    const handleIncrement = (productId: string, currentQuantity: number) => {
        updateCartQuantity(productId, currentQuantity + 1);
    };

    /**
     * Handle quantity decrement
     */
    const handleDecrement = (productId: string, currentQuantity: number) => {
        if (currentQuantity > 1) {
            updateCartQuantity(productId, currentQuantity - 1);
        } else {
            removeFromCart(productId);
        }
    };

    /**
     * Handle item removal
     */
    const handleRemoveItem = (productId: string) => {
        removeFromCart(productId);
    };

    /**
     * Handle clear cart
     */
    const handleClearCart = () => {
        if (window.confirm('Are you sure you want to clear the cart?')) {
            clearCart();
        }
    };

    return (
        <Card className={`h-full flex flex-col ${className}`}>
            <CardHeader className="pb-4">
                <div className="flex items-center justify-between">
                    <CardTitle className="text-lg">Current Order</CardTitle>
                    {!isEmpty && (
                        <Button
                            variant="outline"
                            size="sm"
                            onClick={handleClearCart}
                            aria-label="Clear all items from cart"
                        >
                            Clear All
                        </Button>
                    )}
                </div>
            </CardHeader>

            <CardContent className="flex-1 flex flex-col p-0 overflow-auto">
                {isEmpty ? (
                    <EmptyCartMessage />
                ) : (
                    <>
                        {/* Cart Items */}
                        <ScrollArea className="flex-1 px-6 overflow-auto">
                            <Table className="overflow-auto">
                                <TableHeader>
                                    <TableRow>
                                        <TableHead>Item</TableHead>
                                        <TableHead className="text-center">Qty</TableHead>
                                        <TableHead className="text-right">Price</TableHead>
                                        <TableHead className="w-12"></TableHead>
                                    </TableRow>
                                </TableHeader>
                                <TableBody>
                                    {cart.map((item) => (
                                        <CartItemRow
                                            key={item.product.id}
                                            item={item}
                                            onQuantityChange={handleQuantityChange}
                                            onIncrement={handleIncrement}
                                            onDecrement={handleDecrement}
                                            onRemove={handleRemoveItem}
                                        />
                                    ))}
                                </TableBody>
                            </Table>
                        </ScrollArea>

                        {/* Cart Summary */}
                        <CartSummary
                            subtotal={formattedSubtotal}
                            tax={formattedTax}
                            total={formattedTotal}
                            onCheckout={onCheckout}
                            isEmpty={isEmpty}
                        />
                    </>
                )}
            </CardContent>
        </Card>
    );
}

export default Cart; 