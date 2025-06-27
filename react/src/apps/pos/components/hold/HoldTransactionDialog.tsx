import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Badge } from '@/components/ui/badge';
import { Card, CardContent } from '@/components/ui/card';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Pause, Play, Clock, User, Trash2 } from 'lucide-react';
import type { CartItem, Customer, HeldTransaction } from '../../types/pos';

interface HoldTransactionDialogProps {
    isOpen: boolean;
    onClose: () => void;
    cartItems: CartItem[];
    currentCustomer: Customer | null;
    heldTransactions: HeldTransaction[];
    onHoldTransaction: (note: string) => void;
    onResumeTransaction: (heldTransaction: HeldTransaction) => void;
    onDeleteHeldTransaction: (transactionId: string) => void;
}

const formatPrice = (amount: number) => `$${amount.toFixed(2)}`;

export function HoldTransactionDialog({
    isOpen,
    onClose,
    cartItems,
    currentCustomer,
    heldTransactions,
    onHoldTransaction,
    onResumeTransaction,
    onDeleteHeldTransaction
}: HoldTransactionDialogProps) {
    const [holdNote, setHoldNote] = useState('');

    const canHoldCurrentTransaction = cartItems.length > 0;

    const getCartTotal = () => {
        return cartItems.reduce((sum, item) =>
            sum + (item.product.price * item.quantity), 0
        );
    };

    const getHeldTransactionTotal = (transaction: HeldTransaction) => {
        return transaction.items.reduce((sum, item) =>
            sum + (item.product.price * item.quantity), 0
        );
    };

    const handleHoldTransaction = () => {
        if (!canHoldCurrentTransaction) {
            alert('No items in cart to hold');
            return;
        }

        onHoldTransaction(holdNote.trim() || 'Customer transaction on hold');
        setHoldNote('');
        onClose();
    };

    const handleResumeTransaction = (transaction: HeldTransaction) => {
        onResumeTransaction(transaction);
        onClose();
    };

    const formatDateTime = (date: Date) => {
        return date.toLocaleString();
    };

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Pause className="h-5 w-5" />
                        Hold & Resume Transactions
                    </DialogTitle>
                    <DialogDescription>
                        Hold current transaction or resume a previously held transaction
                    </DialogDescription>
                </DialogHeader>

                <Tabs defaultValue="hold" className="space-y-4">
                    <TabsList className="grid w-full grid-cols-2">
                        <TabsTrigger value="hold">Hold Transaction</TabsTrigger>
                        <TabsTrigger value="resume">Resume Transaction</TabsTrigger>
                    </TabsList>

                    {/* Hold Transaction Tab */}
                    <TabsContent value="hold" className="space-y-4">
                        {canHoldCurrentTransaction ? (
                            <>
                                {/* Current Transaction Summary */}
                                <Card>
                                    <CardContent className="p-4">
                                        <div className="space-y-3">
                                            <div className="flex justify-between items-center">
                                                <span className="font-medium">Current Transaction</span>
                                                <Badge variant="secondary">
                                                    {cartItems.length} item{cartItems.length !== 1 ? 's' : ''}
                                                </Badge>
                                            </div>

                                            {currentCustomer && (
                                                <div className="text-sm text-muted-foreground">
                                                    Customer: {currentCustomer.name}
                                                </div>
                                            )}

                                            <div className="space-y-2 max-h-32 overflow-y-auto">
                                                {cartItems.map(item => (
                                                    <div key={item.product.id} className="flex justify-between text-sm">
                                                        <span>{item.product.name} × {item.quantity}</span>
                                                        <span>{formatPrice(item.product.price * item.quantity)}</span>
                                                    </div>
                                                ))}
                                            </div>

                                            <div className="border-t pt-2">
                                                <div className="flex justify-between font-medium">
                                                    <span>Total:</span>
                                                    <span>{formatPrice(getCartTotal())}</span>
                                                </div>
                                            </div>
                                        </div>
                                    </CardContent>
                                </Card>

                                {/* Hold Note */}
                                <div className="space-y-2">
                                    <Label htmlFor="hold-note">Hold Note (Optional)</Label>
                                    <Input
                                        id="hold-note"
                                        placeholder="Reason for holding transaction..."
                                        value={holdNote}
                                        onChange={(e) => setHoldNote(e.target.value)}
                                    />
                                </div>

                                <Button onClick={handleHoldTransaction} className="w-full">
                                    <Pause className="h-4 w-4 mr-2" />
                                    Hold Transaction
                                </Button>
                            </>
                        ) : (
                            <div className="text-center py-8 text-muted-foreground">
                                <Pause className="h-12 w-12 mx-auto mb-4 opacity-50" />
                                <div className="font-medium">No Transaction to Hold</div>
                                <div className="text-sm">Add items to cart first</div>
                            </div>
                        )}
                    </TabsContent>

                    {/* Resume Transaction Tab */}
                    <TabsContent value="resume" className="space-y-4">
                        {heldTransactions.length > 0 ? (
                            <div className="space-y-3">
                                <div className="flex justify-between items-center">
                                    <Label>Held Transactions</Label>
                                    <Badge variant="outline">
                                        {heldTransactions.length} held
                                    </Badge>
                                </div>

                                <div className="space-y-3 max-h-96 overflow-y-auto">
                                    {heldTransactions.map(transaction => (
                                        <Card key={transaction.id}>
                                            <CardContent className="p-4">
                                                <div className="space-y-3">
                                                    <div className="flex justify-between items-start">
                                                        <div>
                                                            <div className="font-medium flex items-center gap-2">
                                                                <Clock className="h-4 w-4" />
                                                                {formatDateTime(transaction.timestamp)}
                                                            </div>
                                                            {transaction.customer && (
                                                                <div className="text-sm text-muted-foreground flex items-center gap-1">
                                                                    <User className="h-3 w-3" />
                                                                    {transaction.customer.name}
                                                                </div>
                                                            )}
                                                            {transaction.note && (
                                                                <div className="text-sm text-muted-foreground">
                                                                    Note: {transaction.note}
                                                                </div>
                                                            )}
                                                        </div>
                                                        <div className="text-right">
                                                            <div className="font-medium">
                                                                {formatPrice(getHeldTransactionTotal(transaction))}
                                                            </div>
                                                            <div className="text-sm text-muted-foreground">
                                                                {transaction.items.length} item{transaction.items.length !== 1 ? 's' : ''}
                                                            </div>
                                                        </div>
                                                    </div>

                                                    {/* Transaction Items */}
                                                    <div className="space-y-1 max-h-24 overflow-y-auto">
                                                        {transaction.items.map(item => (
                                                            <div key={item.product.id} className="flex justify-between text-xs text-muted-foreground">
                                                                <span>{item.product.name} × {item.quantity}</span>
                                                                <span>{formatPrice(item.product.price * item.quantity)}</span>
                                                            </div>
                                                        ))}
                                                    </div>

                                                    {/* Action Buttons */}
                                                    <div className="flex gap-2">
                                                        <Button
                                                            size="sm"
                                                            className="flex-1"
                                                            onClick={() => handleResumeTransaction(transaction)}
                                                        >
                                                            <Play className="h-3 w-3 mr-1" />
                                                            Resume
                                                        </Button>
                                                        <Button
                                                            size="sm"
                                                            variant="outline"
                                                            onClick={() => {
                                                                if (confirm('Are you sure you want to delete this held transaction?')) {
                                                                    onDeleteHeldTransaction(transaction.id);
                                                                }
                                                            }}
                                                        >
                                                            <Trash2 className="h-3 w-3" />
                                                        </Button>
                                                    </div>
                                                </div>
                                            </CardContent>
                                        </Card>
                                    ))}
                                </div>
                            </div>
                        ) : (
                            <div className="text-center py-8 text-muted-foreground">
                                <Clock className="h-12 w-12 mx-auto mb-4 opacity-50" />
                                <div className="font-medium">No Held Transactions</div>
                                <div className="text-sm">Hold a transaction to see it here</div>
                            </div>
                        )}
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