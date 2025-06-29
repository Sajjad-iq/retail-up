import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Users, Shield } from 'lucide-react';

export function StaffSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Users className="h-5 w-5" />
                        <span>User Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure user accounts, roles, and permissions
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Require employee PIN</Label>
                            <div className="text-sm text-muted-foreground">
                                Staff must enter PIN for transactions
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Track employee sales</Label>
                            <div className="text-sm text-muted-foreground">
                                Monitor individual sales performance
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Allow manager overrides</Label>
                            <div className="text-sm text-muted-foreground">
                                Managers can override system restrictions
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Shield className="h-5 w-5" />
                        <span>Security Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure security policies and access controls
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="sessionTimeout">Session timeout (minutes)</Label>
                            <Input id="sessionTimeout" type="number" placeholder="30" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="passwordLength">Minimum password length</Label>
                            <Input id="passwordLength" type="number" placeholder="8" />
                        </div>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Two-factor authentication</Label>
                            <div className="text-sm text-muted-foreground">
                                Require 2FA for all accounts
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Audit logging</Label>
                            <div className="text-sm text-muted-foreground">
                                Log all user actions for security audit
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 