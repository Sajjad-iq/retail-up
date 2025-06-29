import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Badge } from '@/components/ui/badge';
import { CreditCard, Percent } from 'lucide-react';

export function PaymentsSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <CreditCard className="h-5 w-5" />
                        <span>Payment Methods</span>
                    </CardTitle>
                    <CardDescription>
                        Configure accepted payment methods and processing
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-4">
                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Cash</div>
                                <Badge variant="secondary">Always Available</Badge>
                            </div>
                            <Switch defaultChecked disabled />
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Credit/Debit Cards</div>
                                <Badge variant="outline">Visa, Mastercard, Amex</Badge>
                            </div>
                            <Switch defaultChecked />
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Digital Wallets</div>
                                <Badge variant="outline">Apple Pay, Google Pay</Badge>
                            </div>
                            <Switch />
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Store Credit</div>
                                <Badge variant="outline">Gift Cards, Returns</Badge>
                            </div>
                            <Switch defaultChecked />
                        </div>
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Percent className="h-5 w-5" />
                        <span>Tax Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure tax rates and calculation methods
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="salesTax">Sales Tax Rate (%)</Label>
                            <Input id="salesTax" type="number" step="0.01" placeholder="8.25" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="taxNumber">Tax ID Number</Label>
                            <Input id="taxNumber" placeholder="123-45-6789" />
                        </div>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Tax-inclusive pricing</Label>
                            <div className="text-sm text-muted-foreground">
                                Display prices with tax included
                            </div>
                        </div>
                        <Switch />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 