import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Bell } from 'lucide-react';

export function NotificationsSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Bell className="h-5 w-5" />
                        <span>Notification Preferences</span>
                    </CardTitle>
                    <CardDescription>
                        Configure how and when you receive notifications
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Email notifications</Label>
                            <div className="text-sm text-muted-foreground">
                                Receive important alerts via email
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Push notifications</Label>
                            <div className="text-sm text-muted-foreground">
                                Browser and mobile push notifications
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Low stock alerts</Label>
                            <div className="text-sm text-muted-foreground">
                                Get notified when inventory is running low
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Daily sales summary</Label>
                            <div className="text-sm text-muted-foreground">
                                Receive end-of-day sales reports
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="notificationEmail">Notification Email</Label>
                        <Input id="notificationEmail" type="email" placeholder="notifications@store.com" />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="alertFrequency">Alert Frequency</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select frequency" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="immediate">Immediate</SelectItem>
                                <SelectItem value="hourly">Hourly Digest</SelectItem>
                                <SelectItem value="daily">Daily Digest</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 