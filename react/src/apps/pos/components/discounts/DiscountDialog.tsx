import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Badge } from '@/components/ui/badge';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Separator } from '@/components/ui/separator';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Percent, Tag, Search, Check, X } from 'lucide-react';
import type { Discount, CartItem, DiscountResult } from '../../types/pos';

interface DiscountDialogProps {
    isOpen: boolean;
    onClose: () => void;
    cartItems: CartItem[];
    availableDiscounts: Discount[];
    onApplyDiscount: (discount: Discount, items?: string[]) => DiscountResult;
    onRemoveDiscount: (itemId: string) => void;
}

const formatPrice = (amount: number) => `$${amount.toFixed(2)}`;

export function DiscountDialog({
    isOpen,
    onClose,
    cartItems,
    availableDiscounts,
    onApplyDiscount,
    onRemoveDiscount
}: DiscountDialogProps) {
    const [discountCode, setDiscountCode] = useState('');
    const [customDiscount, setCustomDiscount] = useState({
        type: 'percentage' as 'percentage' | 'fixed',
        value: '',
        reason: ''
    });
    const [selectedItems, setSelectedItems] = useState<string[]>([]);

    const activeDiscounts = availableDiscounts.filter(d => d.isActive);
    const itemsWithDiscounts = cartItems.filter(item => item.discount && item.discount > 0);

    const applyDiscountByCode = () => {
        if (!discountCode.trim()) return;

        const discount = activeDiscounts.find(d =>
            d.name.toLowerCase().includes(discountCode.toLowerCase()) ||
            d.id === discountCode
        );

        if (discount) {
            const result = onApplyDiscount(discount, selectedItems);
            if (result.applied) {
                setDiscountCode('');
                setSelectedItems([]);
            } else {
                alert(result.error || 'Could not apply discount');
            }
        } else {
            alert('Discount code not found or expired');
        }
    };

    const applyCustomDiscount = () => {
        if (!customDiscount.value || !customDiscount.reason.trim()) {
            alert('Please enter discount value and reason');
            return;
        }

        const value = parseFloat(customDiscount.value);
        if (isNaN(value) || value <= 0) {
            alert('Please enter a valid discount value');
            return;
        }

        const discount: Discount = {
            id: `custom-${Date.now()}`,
            name: `Custom ${customDiscount.type === 'percentage' ? value + '%' : formatPrice(value)} Discount`,
            type: customDiscount.type,
            value,
            isActive: true
        };

        const result = onApplyDiscount(discount, selectedItems);
        if (result.applied) {
            setCustomDiscount({ type: 'percentage', value: '', reason: '' });
            setSelectedItems([]);
        } else {
            alert(result.error || 'Could not apply discount');
        }
    };

    const toggleItemSelection = (itemId: string) => {
        setSelectedItems(prev =>
            prev.includes(itemId)
                ? prev.filter(id => id !== itemId)
                : [...prev, itemId]
        );
    };

    const getSubtotal = () => {
        return cartItems.reduce((sum, item) => sum + (item.product.price * item.quantity), 0);
    };

    const getTotalDiscounts = () => {
        return cartItems.reduce((sum, item) => sum + (item.discount || 0), 0);
    };

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-3xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Percent className="h-5 w-5" />
                        Discounts & Promotions
                    </DialogTitle>
                    <DialogDescription>
                        Apply discounts to cart items or enter custom discount codes
                    </DialogDescription>
                </DialogHeader>

                <Tabs defaultValue="apply" className="space-y-4">
                    <TabsList className="grid w-full grid-cols-3">
                        <TabsTrigger value="apply">Apply Discounts</TabsTrigger>
                        <TabsTrigger value="custom">Custom Discount</TabsTrigger>
                        <TabsTrigger value="active">Active Discounts</TabsTrigger>
                    </TabsList>

                    {/* Apply Discounts Tab */}
                    <TabsContent value="apply" className="space-y-4">
                        {/* Discount Code Input */}
                        <div className="space-y-2">
                            <Label>Discount Code</Label>
                            <div className="flex gap-2">
                                <Input
                                    placeholder="Enter discount code..."
                                    value={discountCode}
                                    onChange={(e) => setDiscountCode(e.target.value)}
                                    onKeyPress={(e) => e.key === 'Enter' && applyDiscountByCode()}
                                />
                                <Button onClick={applyDiscountByCode} disabled={!discountCode.trim()}>
                                    <Search className="h-4 w-4 mr-2" />
                                    Apply
                                </Button>
                            </div>
                        </div>

                        {/* Available Promotions */}
                        <div className="space-y-3">
                            <Label>Available Promotions</Label>
                            <div className="grid gap-2 max-h-48 overflow-y-auto">
                                {activeDiscounts.map(discount => (
                                    <Card key={discount.id} className="cursor-pointer hover:bg-muted/50">
                                        <CardContent className="p-3">
                                            <div className="flex items-center justify-between">
                                                <div>
                                                    <div className="font-medium">{discount.name}</div>
                                                    <div className="text-sm text-muted-foreground">
                                                        {discount.type === 'percentage'
                                                            ? `${discount.value}% off`
                                                            : `${formatPrice(discount.value)} off`
                                                        }
                                                        {discount.minAmount && ` (Min: ${formatPrice(discount.minAmount)})`}
                                                    </div>
                                                </div>
                                                <Button
                                                    size="sm"
                                                    onClick={() => {
                                                        const result = onApplyDiscount(discount, selectedItems);
                                                        if (!result.applied) {
                                                            alert(result.error || 'Could not apply discount');
                                                        } else {
                                                            setSelectedItems([]);
                                                        }
                                                    }}
                                                >
                                                    Apply
                                                </Button>
                                            </div>
                                        </CardContent>
                                    </Card>
                                ))}
                                {activeDiscounts.length === 0 && (
                                    <div className="text-center text-muted-foreground py-4">
                                        No active promotions available
                                    </div>
                                )}
                            </div>
                        </div>

                        {/* Item Selection */}
                        {cartItems.length > 0 && (
                            <div className="space-y-3">
                                <Label>Select Items (Optional - leave empty to apply to entire order)</Label>
                                <div className="space-y-2 max-h-40 overflow-y-auto">
                                    {cartItems.map(item => (
                                        <div key={item.product.id} className="flex items-center gap-3">
                                            <input
                                                type="checkbox"
                                                checked={selectedItems.includes(item.product.id)}
                                                onChange={() => toggleItemSelection(item.product.id)}
                                                className="rounded"
                                            />
                                            <div className="flex-1">
                                                <div className="font-medium">{item.product.name}</div>
                                                <div className="text-sm text-muted-foreground">
                                                    {formatPrice(item.product.price)} × {item.quantity}
                                                    {item.discount && item.discount > 0 && (
                                                        <span className="text-green-600 ml-2">
                                                            (Discount: -{formatPrice(item.discount)})
                                                        </span>
                                                    )}
                                                </div>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        )}
                    </TabsContent>

                    {/* Custom Discount Tab */}
                    <TabsContent value="custom" className="space-y-4">
                        <div className="grid grid-cols-2 gap-4">
                            <div className="space-y-2">
                                <Label>Discount Type</Label>
                                <select
                                    value={customDiscount.type}
                                    onChange={(e) => setCustomDiscount(prev => ({
                                        ...prev,
                                        type: e.target.value as 'percentage' | 'fixed'
                                    }))}
                                    className="w-full px-3 py-2 border rounded-md"
                                >
                                    <option value="percentage">Percentage (%)</option>
                                    <option value="fixed">Fixed Amount ($)</option>
                                </select>
                            </div>

                            <div className="space-y-2">
                                <Label>Discount Value</Label>
                                <Input
                                    type="number"
                                    placeholder={customDiscount.type === 'percentage' ? '10' : '5.00'}
                                    value={customDiscount.value}
                                    onChange={(e) => setCustomDiscount(prev => ({
                                        ...prev,
                                        value: e.target.value
                                    }))}
                                />
                            </div>
                        </div>

                        <div className="space-y-2">
                            <Label>Reason for Discount</Label>
                            <Input
                                placeholder="Manager override, price match, etc."
                                value={customDiscount.reason}
                                onChange={(e) => setCustomDiscount(prev => ({
                                    ...prev,
                                    reason: e.target.value
                                }))}
                            />
                        </div>

                        <Button
                            onClick={applyCustomDiscount}
                            className="w-full"
                            disabled={!customDiscount.value || !customDiscount.reason.trim()}
                        >
                            Apply Custom Discount
                        </Button>
                    </TabsContent>

                    {/* Active Discounts Tab */}
                    <TabsContent value="active" className="space-y-4">
                        <div className="space-y-3">
                            <div className="flex justify-between items-center">
                                <Label>Items with Applied Discounts</Label>
                                <Badge variant="secondary">
                                    Total Savings: {formatPrice(getTotalDiscounts())}
                                </Badge>
                            </div>

                            {itemsWithDiscounts.length > 0 ? (
                                <div className="space-y-2">
                                    {itemsWithDiscounts.map(item => (
                                        <Card key={item.product.id}>
                                            <CardContent className="p-3">
                                                <div className="flex items-center justify-between">
                                                    <div>
                                                        <div className="font-medium">{item.product.name}</div>
                                                        <div className="text-sm text-muted-foreground">
                                                            Original: {formatPrice(item.product.price * item.quantity)} •
                                                            Discount: -{formatPrice(item.discount || 0)}
                                                        </div>
                                                    </div>
                                                    <Button
                                                        variant="outline"
                                                        size="sm"
                                                        onClick={() => onRemoveDiscount(item.product.id)}
                                                    >
                                                        <X className="h-3 w-3 mr-1" />
                                                        Remove
                                                    </Button>
                                                </div>
                                            </CardContent>
                                        </Card>
                                    ))}
                                </div>
                            ) : (
                                <div className="text-center text-muted-foreground py-8">
                                    No discounts currently applied
                                </div>
                            )}

                            {/* Order Summary */}
                            {cartItems.length > 0 && (
                                <Card>
                                    <CardHeader>
                                        <CardTitle className="text-base">Order Summary</CardTitle>
                                    </CardHeader>
                                    <CardContent className="space-y-2">
                                        <div className="flex justify-between">
                                            <span>Subtotal:</span>
                                            <span>{formatPrice(getSubtotal())}</span>
                                        </div>
                                        <div className="flex justify-between text-green-600">
                                            <span>Total Discounts:</span>
                                            <span>-{formatPrice(getTotalDiscounts())}</span>
                                        </div>
                                        <Separator />
                                        <div className="flex justify-between font-medium text-lg">
                                            <span>After Discounts:</span>
                                            <span>{formatPrice(getSubtotal() - getTotalDiscounts())}</span>
                                        </div>
                                    </CardContent>
                                </Card>
                            )}
                        </div>
                    </TabsContent>
                </Tabs>

                <DialogFooter>
                    <Button variant="outline" onClick={onClose}>
                        Close
                    </Button>
                </DialogFooter>
            </DialogContent>
        </Dialog>
    );
} 