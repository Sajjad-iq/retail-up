import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Button } from '@/components/ui/button';
import { HardDrive, Download } from 'lucide-react';

export function SystemSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <HardDrive className="h-5 w-5" />
                        <span>Data Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure backup, data retention, and system maintenance
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Automatic backups</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically backup data daily
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="retentionPeriod">Data retention period (days)</Label>
                        <Input id="retentionPeriod" type="number" placeholder="365" />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Cloud sync</Label>
                            <div className="text-sm text-muted-foreground">
                                Sync data with cloud storage
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="pt-4">
                        <Button variant="outline" className="w-full">
                            <Download className="mr-2 h-4 w-4" />
                            Backup Data Now
                        </Button>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 