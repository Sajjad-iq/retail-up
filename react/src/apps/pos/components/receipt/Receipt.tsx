import React from 'react';
import { Card, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';
import { Printer, Download, Mail } from 'lucide-react';
import type { Transaction, ReceiptConfig } from '../../types/pos';

interface ReceiptProps {
    transaction: Transaction;
    config?: ReceiptConfig;
    onPrint?: () => void;
    onEmail?: () => void;
    onDownload?: () => void;
    className?: string;
}

const defaultConfig: ReceiptConfig = {
    storeName: 'RetailUp Store',
    storeAddress: '123 Main Street, City, State 12345',
    storePhone: '(555) 123-4567',
    taxId: 'TAX-123456789',
    footerMessage: 'Thank you for your business!'
};

// Simple formatters
const formatPrice = (amount: number) => `$${amount.toFixed(2)}`;
const formatDateTime = (date: Date) => date.toLocaleString();

export function Receipt({
    transaction,
    config = defaultConfig,
    onPrint,
    onEmail,
    onDownload,
    className
}: ReceiptProps) {
    const receiptNumber = transaction.receiptNumber || transaction.id;

    const handlePrint = () => {
        const printContent = document.getElementById('receipt-content');
        if (printContent) {
            const printWindow = window.open('', '_blank');
            if (printWindow) {
                printWindow.document.write(`
          <!DOCTYPE html>
          <html>
            <head>
              <title>Receipt - ${receiptNumber}</title>
              <style>
                body { 
                  font-family: 'Courier New', monospace; 
                  margin: 0; 
                  padding: 20px; 
                  font-size: 12px;
                  line-height: 1.4;
                }
                .receipt { 
                  max-width: 300px; 
                  margin: 0 auto; 
                }
                .center { text-align: center; }
                .item-row { 
                  display: flex; 
                  justify-content: space-between; 
                  margin: 5px 0; 
                }
                .header { font-size: 14px; font-weight: bold; }
                .total { font-size: 14px; font-weight: bold; }
                .separator { border-bottom: 1px dashed #000; margin: 10px 0; }
                @media print {
                  body { margin: 0; }
                }
              </style>
            </head>
            <body>
              ${printContent.innerHTML}
            </body>
          </html>
        `);
                printWindow.document.close();
                printWindow.print();
            }
        }
        onPrint?.();
    };

    return (
        <Card className={className}>
            <CardContent className="p-6">
                {/* Action Buttons */}
                <div className="flex gap-2 mb-4">
                    <Button onClick={handlePrint} size="sm" className="flex-1">
                        <Printer className="h-4 w-4 mr-2" />
                        Print
                    </Button>
                    {onEmail && (
                        <Button onClick={onEmail} variant="outline" size="sm" className="flex-1">
                            <Mail className="h-4 w-4 mr-2" />
                            Email
                        </Button>
                    )}
                    {onDownload && (
                        <Button onClick={onDownload} variant="outline" size="sm" className="flex-1">
                            <Download className="h-4 w-4 mr-2" />
                            Download
                        </Button>
                    )}
                </div>

                {/* Receipt Preview */}
                <div id="receipt-content" className="font-mono text-sm space-y-2 border rounded-lg p-4 bg-white" style={{ maxWidth: '300px', margin: '0 auto' }}>
                    {/* Header */}
                    <div className="text-center space-y-1">
                        <div className="font-bold text-base">{config.storeName}</div>
                        <div className="text-xs">{config.storeAddress}</div>
                        <div className="text-xs">{config.storePhone}</div>
                        {config.taxId && <div className="text-xs">Tax ID: {config.taxId}</div>}
                    </div>

                    <Separator className="my-2" />

                    {/* Transaction Info */}
                    <div className="text-center space-y-1">
                        <div className="text-xs">Receipt #: {receiptNumber}</div>
                        <div className="text-xs">{formatDateTime(transaction.timestamp)}</div>
                        {transaction.cashier && <div className="text-xs">Cashier: {transaction.cashier}</div>}
                    </div>

                    <Separator className="my-2" />

                    {/* Items */}
                    <div className="space-y-1">
                        {transaction.items.map((item, index) => (
                            <div key={index} className="space-y-1">
                                <div className="text-xs">{item.product.name}</div>
                                <div className="flex justify-between text-xs">
                                    <span>{item.quantity} x {formatPrice(item.customPrice || item.product.price)}</span>
                                    <span>{formatPrice((item.customPrice || item.product.price) * item.quantity)}</span>
                                </div>
                                {item.discount && (
                                    <div className="flex justify-between text-xs text-red-600">
                                        <span className="ml-2">Discount</span>
                                        <span>-{formatPrice(item.discount)}</span>
                                    </div>
                                )}
                            </div>
                        ))}
                    </div>

                    <Separator className="my-2" />

                    {/* Totals */}
                    <div className="space-y-1">
                        <div className="flex justify-between text-xs">
                            <span>Subtotal:</span>
                            <span>{formatPrice(transaction.subtotal)}</span>
                        </div>

                        {transaction.discount > 0 && (
                            <div className="flex justify-between text-xs text-red-600">
                                <span>Total Discount:</span>
                                <span>-{formatPrice(transaction.discount)}</span>
                            </div>
                        )}

                        <div className="flex justify-between text-xs">
                            <span>Tax:</span>
                            <span>{formatPrice(transaction.tax)}</span>
                        </div>
                    </div>

                    <Separator className="my-2" />

                    <div className="flex justify-between text-sm font-bold">
                        <span>TOTAL:</span>
                        <span>{formatPrice(transaction.total)}</span>
                    </div>

                    <Separator className="my-2" />

                    {/* Payments */}
                    <div className="space-y-1">
                        {transaction.payments.map((payment, index) => (
                            <div key={index} className="space-y-1">
                                <div className="flex justify-between text-xs">
                                    <span>{payment.method.toUpperCase()}:</span>
                                    <span>{formatPrice(payment.amount)}</span>
                                </div>
                                {payment.change && (
                                    <div className="flex justify-between text-xs">
                                        <span>Change:</span>
                                        <span>{formatPrice(payment.change)}</span>
                                    </div>
                                )}
                            </div>
                        ))}
                    </div>

                    {/* Customer Info */}
                    {transaction.customer && (
                        <>
                            <Separator className="my-2" />
                            <div className="text-center text-xs space-y-1">
                                <div>Customer: {transaction.customer.name}</div>
                                {transaction.customer.phone && <div>Phone: {transaction.customer.phone}</div>}
                                {transaction.customer.email && <div>Email: {transaction.customer.email}</div>}
                            </div>
                        </>
                    )}

                    {/* Footer */}
                    {config.footerMessage && (
                        <>
                            <Separator className="my-2" />
                            <div className="text-center text-xs">{config.footerMessage}</div>
                        </>
                    )}

                    <div className="text-center text-xs mt-4 text-muted-foreground">
                        RetailUp POS System
                    </div>
                </div>
            </CardContent>
        </Card>
    );
} 