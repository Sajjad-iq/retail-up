import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { UserCheck } from 'lucide-react';

export function CustomersSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <UserCheck className="h-5 w-5" />
                        <span>Customer Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure customer data collection and loyalty programs
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Collect customer emails</Label>
                            <div className="text-sm text-muted-foreground">
                                Ask for email addresses during checkout
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Loyalty program</Label>
                            <div className="text-sm text-muted-foreground">
                                Enable points-based loyalty rewards
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="pointsPerDollar">Points per dollar spent</Label>
                            <Input id="pointsPerDollar" type="number" step="0.1" placeholder="1.0" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="pointValue">Point value (cents)</Label>
                            <Input id="pointValue" type="number" step="0.1" placeholder="1.0" />
                        </div>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Marketing emails</Label>
                            <div className="text-sm text-muted-foreground">
                                Send promotional emails to customers
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Birthday rewards</Label>
                            <div className="text-sm text-muted-foreground">
                                Send special offers on customer birthdays
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 