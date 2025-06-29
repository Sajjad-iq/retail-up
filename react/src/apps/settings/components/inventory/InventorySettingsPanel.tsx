import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Package, Database } from 'lucide-react';

export function InventorySettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Package className="h-5 w-5" />
                        <span>Stock Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure inventory tracking and stock alert settings
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Track inventory automatically</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically update stock levels on sales
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Low stock alerts</Label>
                            <div className="text-sm text-muted-foreground">
                                Send notifications when items are running low
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="lowStockThreshold">Low stock threshold</Label>
                            <Input id="lowStockThreshold" type="number" placeholder="10" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="criticalStockThreshold">Critical stock threshold</Label>
                            <Input id="criticalStockThreshold" type="number" placeholder="3" />
                        </div>
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Database className="h-5 w-5" />
                        <span>Product Management</span>
                    </CardTitle>
                    <CardDescription>
                        Settings for product creation and management
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Auto-generate SKUs</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically create SKU codes for new products
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Require product images</Label>
                            <div className="text-sm text-muted-foreground">
                                Make product images mandatory for new items
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="defaultCategory">Default product category</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select default category" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="general">General</SelectItem>
                                <SelectItem value="electronics">Electronics</SelectItem>
                                <SelectItem value="clothing">Clothing</SelectItem>
                                <SelectItem value="food">Food & Beverage</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 