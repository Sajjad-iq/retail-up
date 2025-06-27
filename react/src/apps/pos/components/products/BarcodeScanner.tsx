import { useState, useRef, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Scan, X, Check, AlertCircle } from 'lucide-react';
import { useProducts } from '../../hooks/use-pos';
import type { Product, BarcodeScanResult } from '../../types/pos';

interface BarcodeScannerProps {
    onProductScanned: (product: Product) => void;
    onClose?: () => void;
    className?: string;
}

export function BarcodeScanner({ onProductScanned, onClose, className }: BarcodeScannerProps) {
    const [manualBarcode, setManualBarcode] = useState('');
    const [scanResult, setScanResult] = useState<BarcodeScanResult | null>(null);
    const [isScanning, setIsScanning] = useState(false);
    const inputRef = useRef<HTMLInputElement>(null);

    const { products } = useProducts();

    useEffect(() => {
        // Auto-focus the input for quick manual entry
        if (inputRef.current) {
            inputRef.current.focus();
        }
    }, []);

    const findProductByBarcode = (barcode: string): Product | undefined => {
        return products.find(product =>
            product.barcode === barcode ||
            product.id === barcode ||
            product.sku === barcode
        );
    };

    const handleManualScan = () => {
        if (!manualBarcode.trim()) return;

        const product = findProductByBarcode(manualBarcode.trim());
        const result: BarcodeScanResult = {
            code: manualBarcode.trim(),
            product,
            timestamp: new Date()
        };

        setScanResult(result);

        if (product) {
            onProductScanned(product);
            setManualBarcode('');
            setScanResult(null);
        }
    };

    const handleInputKeyPress = (e: React.KeyboardEvent) => {
        if (e.key === 'Enter') {
            handleManualScan();
        }
    };

    const startCameraScanning = async () => {
        setIsScanning(true);
        // In a real implementation, you would integrate with a barcode scanning library
        // like @zxing/library or quagga2 for camera-based scanning

        // Simulate camera scanning for now
        setTimeout(() => {
            setIsScanning(false);
            // This would be replaced with actual camera scanning logic
            alert('Camera scanning would be implemented here using a barcode scanning library');
        }, 2000);
    };

    const clearResult = () => {
        setScanResult(null);
        setManualBarcode('');
        if (inputRef.current) {
            inputRef.current.focus();
        }
    };

    return (
        <Card className={className}>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-4">
                <CardTitle className="text-lg font-semibold flex items-center gap-2">
                    <Scan className="h-5 w-5" />
                    Barcode Scanner
                </CardTitle>
                {onClose && (
                    <Button variant="ghost" size="sm" onClick={onClose}>
                        <X className="h-4 w-4" />
                    </Button>
                )}
            </CardHeader>

            <CardContent className="space-y-4">
                {/* Manual Barcode Entry */}
                <div className="space-y-2">
                    <label className="text-sm font-medium">Manual Entry</label>
                    <div className="flex gap-2">
                        <Input
                            ref={inputRef}
                            placeholder="Enter barcode, SKU, or product ID..."
                            value={manualBarcode}
                            onChange={(e) => setManualBarcode(e.target.value)}
                            onKeyPress={handleInputKeyPress}
                            className="flex-1"
                        />
                        <Button
                            onClick={handleManualScan}
                            disabled={!manualBarcode.trim()}
                        >
                            Scan
                        </Button>
                    </div>
                </div>

                {/* Camera Scanning */}
                <div className="space-y-2">
                    <label className="text-sm font-medium">Camera Scanning</label>
                    <Button
                        variant="outline"
                        onClick={startCameraScanning}
                        disabled={isScanning}
                        className="w-full"
                    >
                        {isScanning ? (
                            <>
                                <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-current mr-2" />
                                Scanning...
                            </>
                        ) : (
                            <>
                                <Scan className="h-4 w-4 mr-2" />
                                Start Camera Scan
                            </>
                        )}
                    </Button>
                </div>

                {/* Scan Result */}
                {scanResult && (
                    <div className="border rounded-lg p-4 space-y-3">
                        <div className="flex items-center justify-between">
                            <span className="text-sm font-medium">Scan Result</span>
                            <Button variant="ghost" size="sm" onClick={clearResult}>
                                <X className="h-3 w-3" />
                            </Button>
                        </div>

                        <div className="space-y-2">
                            <div className="flex items-center gap-2">
                                <span className="text-xs text-muted-foreground">Code:</span>
                                <code className="text-xs bg-muted px-1 py-0.5 rounded">{scanResult.code}</code>
                            </div>

                            {scanResult.product ? (
                                <div className="space-y-2">
                                    <div className="flex items-center gap-2">
                                        <Check className="h-4 w-4 text-green-500" />
                                        <span className="text-sm font-medium text-green-700">Product Found</span>
                                    </div>

                                    <div className="bg-green-50 border border-green-200 rounded-lg p-3">
                                        <div className="font-medium">{scanResult.product.name}</div>
                                        <div className="text-sm text-muted-foreground">
                                            ${scanResult.product.price.toFixed(2)} • {scanResult.product.category}
                                        </div>
                                        <div className="text-xs text-muted-foreground mt-1">
                                            Stock: {scanResult.product.stock} units
                                        </div>
                                        {scanResult.product.sku && (
                                            <div className="text-xs text-muted-foreground">
                                                SKU: {scanResult.product.sku}
                                            </div>
                                        )}
                                    </div>

                                    <Button
                                        onClick={() => {
                                            onProductScanned(scanResult.product!);
                                            clearResult();
                                        }}
                                        className="w-full"
                                        size="sm"
                                    >
                                        Add to Cart
                                    </Button>
                                </div>
                            ) : (
                                <div className="space-y-2">
                                    <div className="flex items-center gap-2">
                                        <AlertCircle className="h-4 w-4 text-red-500" />
                                        <span className="text-sm font-medium text-red-700">Product Not Found</span>
                                    </div>

                                    <div className="bg-red-50 border border-red-200 rounded-lg p-3">
                                        <div className="text-sm text-red-700">
                                            No product found with barcode "{scanResult.code}"
                                        </div>
                                        <div className="text-xs text-red-600 mt-1">
                                            Please check the barcode or add this product to inventory.
                                        </div>
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>
                )}

                {/* Quick Instructions */}
                <div className="text-xs text-muted-foreground space-y-1">
                    <div>• Type or scan barcode to find products</div>
                    <div>• Press Enter after typing to scan</div>
                    <div>• Supports barcodes, SKUs, and product IDs</div>
                </div>
            </CardContent>
        </Card>
    );
} 