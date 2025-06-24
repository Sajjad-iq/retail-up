import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Separator } from '@/components/ui/separator';
import {
    Table,
    TableBody,
    TableCell,
    TableHead,
    TableHeader,
    TableRow,
} from '@/components/ui/table';
import { Trash2, Plus, Minus } from 'lucide-react';
import { useCart } from '../hooks/use-pos';
import { formatPrice } from '../lib/utils/pos-utils';
import type { CartItem } from '../types/pos';

interface CartProps {
    /** CSS class name for styling */
    className?: string;
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
export function Cart({ className }: CartProps) {
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

            <CardContent className="flex-1 flex flex-col p-0">
                {isEmpty ? (
                    <EmptyCartMessage />
                ) : (
                    <>
                        {/* Cart Items */}
                        <ScrollArea className="flex-1 px-6">
                            <Table>
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
                        />
                    </>
                )}
            </CardContent>
        </Card>
    );
}

/**
 * Empty cart message component
 */
function EmptyCartMessage() {
    return (
        <div className="flex-1 flex items-center justify-center text-center p-8">
            <div className="text-muted-foreground">
                <p className="text-lg mb-2">Cart is empty</p>
                <p className="text-sm">Add items from the product grid</p>
            </div>
        </div>
    );
}

/**
 * Cart item row component
 */
interface CartItemRowProps {
    item: CartItem;
    onQuantityChange: (productId: string, value: string) => void;
    onIncrement: (productId: string, currentQuantity: number) => void;
    onDecrement: (productId: string, currentQuantity: number) => void;
    onRemove: (productId: string) => void;
}

function CartItemRow({
    item,
    onQuantityChange,
    onIncrement,
    onDecrement,
    onRemove,
}: CartItemRowProps) {
    return (
        <TableRow>
            <TableCell className="font-medium">
                <div>
                    <p className="font-semibold">{item.product.name}</p>
                    <p className="text-sm text-muted-foreground">
                        {formatPrice(item.product.price)} each
                    </p>
                </div>
            </TableCell>

            <TableCell className="text-center">
                <div className="flex items-center justify-center gap-2">
                    <Button
                        variant="outline"
                        size="sm"
                        className="h-6 w-6 p-0"
                        onClick={() => onDecrement(item.product.id, item.quantity)}
                        aria-label="Decrease quantity"
                    >
                        <Minus className="h-3 w-3" />
                    </Button>

                    <Input
                        type="number"
                        min="1"
                        value={item.quantity}
                        onChange={(e) => onQuantityChange(item.product.id, e.target.value)}
                        className="w-16 h-6 text-center text-xs"
                        aria-label={`Quantity for ${item.product.name}`}
                    />

                    <Button
                        variant="outline"
                        size="sm"
                        className="h-6 w-6 p-0"
                        onClick={() => onIncrement(item.product.id, item.quantity)}
                        aria-label="Increase quantity"
                    >
                        <Plus className="h-3 w-3" />
                    </Button>
                </div>
            </TableCell>

            <TableCell className="text-right font-semibold">
                {formatPrice(item.product.price * item.quantity)}
            </TableCell>

            <TableCell>
                <Button
                    variant="ghost"
                    size="sm"
                    className="h-6 w-6 p-0 text-destructive hover:text-destructive"
                    onClick={() => onRemove(item.product.id)}
                    aria-label={`Remove ${item.product.name} from cart`}
                >
                    <Trash2 className="h-3 w-3" />
                </Button>
            </TableCell>
        </TableRow>
    );
}

/**
 * Cart summary component
 */
interface CartSummaryProps {
    subtotal: string;
    tax: string;
    total: string;
}

function CartSummary({ subtotal, tax, total }: CartSummaryProps) {
    return (
        <div className="border-t p-6">
            <div className="space-y-2">
                <div className="flex justify-between text-sm">
                    <span>Subtotal:</span>
                    <span>{subtotal}</span>
                </div>
                <div className="flex justify-between text-sm">
                    <span>Tax (8%):</span>
                    <span>{tax}</span>
                </div>
                <Separator />
                <div className="flex justify-between text-lg font-bold">
                    <span>Total:</span>
                    <span>{total}</span>
                </div>
            </div>
        </div>
    );
}

export default Cart; 