import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Textarea } from '@/components/ui/textarea';
import { Button } from '@/components/ui/button';
import { Receipt, Printer, Scan } from 'lucide-react';

export function POSSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Receipt className="h-5 w-5" />
                        <span>Receipt Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure receipt appearance and printing options
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Print receipts automatically</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically print receipts after each transaction
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Email receipts</Label>
                            <div className="text-sm text-muted-foreground">
                                Offer to email receipts to customers
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="receiptFooter">Receipt Footer Text</Label>
                        <Textarea
                            id="receiptFooter"
                            placeholder="Thank you for your business!&#10;Visit us again soon."
                            className="min-h-[60px]"
                        />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="logoUrl">Logo URL (Optional)</Label>
                        <Input id="logoUrl" placeholder="https://yourstore.com/logo.png" />
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Printer className="h-5 w-5" />
                        <span>Hardware Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure connected POS hardware and devices
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Barcode Scanner</Label>
                            <div className="text-sm text-muted-foreground">
                                Enable barcode scanning functionality
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Cash Drawer</Label>
                            <div className="text-sm text-muted-foreground">
                                Connect and control cash drawer
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Customer Display</Label>
                            <div className="text-sm text-muted-foreground">
                                Show order details to customer
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="printerPort">Receipt Printer Port</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select printer port" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="COM1">COM1</SelectItem>
                                <SelectItem value="COM2">COM2</SelectItem>
                                <SelectItem value="USB">USB</SelectItem>
                                <SelectItem value="NETWORK">Network</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Scan className="h-5 w-5" />
                        <span>Interface Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Customize the POS interface and user experience
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-2">
                        <Label htmlFor="gridSize">Product Grid Size</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select grid size" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="small">Small (More products)</SelectItem>
                                <SelectItem value="medium">Medium (Balanced)</SelectItem>
                                <SelectItem value="large">Large (Larger buttons)</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Show product images</Label>
                            <div className="text-sm text-muted-foreground">
                                Display product images in the POS grid
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Enable quick keys</Label>
                            <div className="text-sm text-muted-foreground">
                                Allow keyboard shortcuts for common actions
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Sound effects</Label>
                            <div className="text-sm text-muted-foreground">
                                Play sounds for scans and transactions
                            </div>
                        </div>
                        <Switch />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 