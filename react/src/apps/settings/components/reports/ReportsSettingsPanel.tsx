import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { BarChart3 } from 'lucide-react';

export function ReportsSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <BarChart3 className="h-5 w-5" />
                        <span>Report Preferences</span>
                    </CardTitle>
                    <CardDescription>
                        Configure report generation and analytics settings
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-2">
                        <Label htmlFor="defaultPeriod">Default report period</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select period" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="today">Today</SelectItem>
                                <SelectItem value="week">This Week</SelectItem>
                                <SelectItem value="month">This Month</SelectItem>
                                <SelectItem value="quarter">This Quarter</SelectItem>
                                <SelectItem value="year">This Year</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Auto-generate daily reports</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically create end-of-day reports
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="reportEmail">Email reports to</Label>
                        <Input id="reportEmail" type="email" placeholder="manager@store.com" />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 