import { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Badge } from '@/components/ui/badge';
import { Textarea } from '@/components/ui/textarea';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { Card, CardContent } from '@/components/ui/card';
import { Separator } from '@/components/ui/separator';
import { RotateCcw, Search, Minus, Plus } from 'lucide-react';
import type { Transaction, ReturnFormData, PaymentMethod, ReturnTransaction } from '../../types/pos';

interface ReturnDialogProps {
    isOpen: boolean;
    onClose: () => void;
    onReturn: (returnData: ReturnFormData) => Promise<ReturnTransaction>;
    transactions: Transaction[];
}

interface ReturnItem {
    productId: string;
    productName: string;
    originalQuantity: number;
    returnQuantity: number;
    unitPrice: number;
    reason: string;
}

const returnReasons = [
    'Defective/Damaged',
    'Wrong Item',
    'Customer Changed Mind',
    'Size/Fit Issue',
    'Not as Described',
    'Other'
];

const formatPrice = (amount: number) => `$${amount.toFixed(2)}`;

export function ReturnDialog({ isOpen, onClose, onReturn, transactions }: ReturnDialogProps) {
    const [receiptNumber, setReceiptNumber] = useState('');
    const [selectedTransaction, setSelectedTransaction] = useState<Transaction | null>(null);
    const [returnItems, setReturnItems] = useState<ReturnItem[]>([]);
    const [refundMethod, setRefundMethod] = useState<PaymentMethod>('cash');
    const [generalReason, setGeneralReason] = useState('');
    const [isProcessing, setIsProcessing] = useState(false);

    useEffect(() => {
        if (!isOpen) {
            setReceiptNumber('');
            setSelectedTransaction(null);
            setReturnItems([]);
            setRefundMethod('cash');
            setGeneralReason('');
            setIsProcessing(false);
        }
    }, [isOpen]);

    const searchTransaction = () => {
        if (!receiptNumber.trim()) return;

        const transaction = transactions.find(t =>
            t.id === receiptNumber.trim() ||
            t.receiptNumber === receiptNumber.trim()
        );

        if (transaction) {
            setSelectedTransaction(transaction);
            setReturnItems(
                transaction.items.map(item => ({
                    productId: item.product.id,
                    productName: item.product.name,
                    originalQuantity: item.quantity,
                    returnQuantity: 0,
                    unitPrice: item.customPrice || item.product.price,
                    reason: ''
                }))
            );
        } else {
            alert('Transaction not found. Please check the receipt number.');
        }
    };

    const updateReturnQuantity = (productId: string, change: number) => {
        setReturnItems(items =>
            items.map(item => {
                if (item.productId === productId) {
                    const newQuantity = Math.max(0, Math.min(item.originalQuantity, item.returnQuantity + change));
                    return { ...item, returnQuantity: newQuantity };
                }
                return item;
            })
        );
    };

    const updateItemReason = (productId: string, reason: string) => {
        setReturnItems(items =>
            items.map(item =>
                item.productId === productId ? { ...item, reason } : item
            )
        );
    };

    const getReturnTotal = () => {
        return returnItems.reduce((sum, item) => sum + (item.returnQuantity * item.unitPrice), 0);
    };

    const getSelectedItems = () => {
        return returnItems.filter(item => item.returnQuantity > 0);
    };

    const processReturn = async () => {
        if (!selectedTransaction) return;

        const selectedItems = getSelectedItems();
        if (selectedItems.length === 0) {
            alert('Please select at least one item to return.');
            return;
        }

        setIsProcessing(true);

        try {
            const returnData: ReturnFormData = {
                originalTransactionId: selectedTransaction.id,
                items: selectedItems.map(item => ({
                    productId: item.productId,
                    quantity: item.returnQuantity,
                    reason: item.reason
                })),
                reason: generalReason,
                refundMethod
            };

            await onReturn(returnData);
            onClose();
        } catch (error) {
            alert('Failed to process return. Please try again.');
        } finally {
            setIsProcessing(false);
        }
    };

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <RotateCcw className="h-5 w-5" />
                        Process Return/Refund
                    </DialogTitle>
                    <DialogDescription>
                        Search for a transaction and select items to return
                    </DialogDescription>
                </DialogHeader>

                <div className="space-y-4">
                    {/* Transaction Search */}
                    <div className="space-y-2">
                        <Label htmlFor="receipt-number">Receipt Number</Label>
                        <div className="flex gap-2">
                            <Input
                                id="receipt-number"
                                placeholder="Enter receipt or transaction number..."
                                value={receiptNumber}
                                onChange={(e) => setReceiptNumber(e.target.value)}
                                onKeyPress={(e) => e.key === 'Enter' && searchTransaction()}
                            />
                            <Button onClick={searchTransaction} disabled={!receiptNumber.trim()}>
                                <Search className="h-4 w-4 mr-2" />
                                Search
                            </Button>
                        </div>
                    </div>

                    {/* Transaction Details */}
                    {selectedTransaction && (
                        <>
                            <Card>
                                <CardContent className="p-4">
                                    <div className="space-y-2">
                                        <div className="flex justify-between items-center">
                                            <span className="font-medium">Transaction Found</span>
                                            <Badge variant="outline">{selectedTransaction.id}</Badge>
                                        </div>
                                        <div className="text-sm text-muted-foreground">
                                            {selectedTransaction.timestamp.toLocaleDateString()} •
                                            Total: {formatPrice(selectedTransaction.total)}
                                        </div>
                                    </div>
                                </CardContent>
                            </Card>

                            {/* Return Items */}
                            <div className="space-y-3">
                                <Label>Select Items to Return</Label>
                                {returnItems.map((item) => (
                                    <Card key={item.productId}>
                                        <CardContent className="p-4">
                                            <div className="space-y-3">
                                                <div className="flex justify-between items-start">
                                                    <div>
                                                        <div className="font-medium">{item.productName}</div>
                                                        <div className="text-sm text-muted-foreground">
                                                            {formatPrice(item.unitPrice)} each • Original qty: {item.originalQuantity}
                                                        </div>
                                                    </div>
                                                    <div className="flex items-center gap-2">
                                                        <Button
                                                            variant="outline"
                                                            size="sm"
                                                            onClick={() => updateReturnQuantity(item.productId, -1)}
                                                            disabled={item.returnQuantity === 0}
                                                        >
                                                            <Minus className="h-3 w-3" />
                                                        </Button>
                                                        <span className="w-8 text-center">{item.returnQuantity}</span>
                                                        <Button
                                                            variant="outline"
                                                            size="sm"
                                                            onClick={() => updateReturnQuantity(item.productId, 1)}
                                                            disabled={item.returnQuantity >= item.originalQuantity}
                                                        >
                                                            <Plus className="h-3 w-3" />
                                                        </Button>
                                                    </div>
                                                </div>

                                                {item.returnQuantity > 0 && (
                                                    <div className="space-y-2">
                                                        <Label>Return Reason</Label>
                                                        <Select
                                                            value={item.reason}
                                                            onValueChange={(value) => updateItemReason(item.productId, value)}
                                                        >
                                                            <SelectTrigger>
                                                                <SelectValue placeholder="Select reason..." />
                                                            </SelectTrigger>
                                                            <SelectContent>
                                                                {returnReasons.map(reason => (
                                                                    <SelectItem key={reason} value={reason}>
                                                                        {reason}
                                                                    </SelectItem>
                                                                ))}
                                                            </SelectContent>
                                                        </Select>
                                                    </div>
                                                )}
                                            </div>
                                        </CardContent>
                                    </Card>
                                ))}
                            </div>

                            {/* Refund Details */}
                            {getSelectedItems().length > 0 && (
                                <>
                                    <div className="space-y-3">
                                        <Label>Refund Method</Label>
                                        <Select value={refundMethod} onValueChange={(value: PaymentMethod) => setRefundMethod(value)}>
                                            <SelectTrigger>
                                                <SelectValue />
                                            </SelectTrigger>
                                            <SelectContent>
                                                <SelectItem value="cash">Cash Refund</SelectItem>
                                            </SelectContent>
                                        </Select>
                                    </div>

                                    <div className="space-y-2">
                                        <Label htmlFor="general-reason">Additional Notes</Label>
                                        <Textarea
                                            id="general-reason"
                                            placeholder="Additional notes about this return..."
                                            value={generalReason}
                                            onChange={(e) => setGeneralReason(e.target.value)}
                                            rows={3}
                                        />
                                    </div>

                                    <Card>
                                        <CardContent className="p-4 space-y-3">
                                            <div className="font-medium">Return Summary</div>
                                            {getSelectedItems().map((item) => (
                                                <div key={item.productId} className="flex justify-between text-sm">
                                                    <span>{item.productName} × {item.returnQuantity}</span>
                                                    <span>{formatPrice(item.returnQuantity * item.unitPrice)}</span>
                                                </div>
                                            ))}
                                            <Separator />
                                            <div className="flex justify-between font-medium">
                                                <span>Total Refund</span>
                                                <span>{formatPrice(getReturnTotal())}</span>
                                            </div>
                                        </CardContent>
                                    </Card>
                                </>
                            )}
                        </>
                    )}
                </div>

                <DialogFooter className="gap-2">
                    <Button variant="outline" onClick={onClose}>
                        Cancel
                    </Button>

                    {selectedTransaction && getSelectedItems().length > 0 && (
                        <Button
                            onClick={processReturn}
                            disabled={isProcessing}
                        >
                            {isProcessing ? 'Processing...' : 'Process Return'}
                        </Button>
                    )}
                </DialogFooter>
            </DialogContent>
        </Dialog>
    );
} 