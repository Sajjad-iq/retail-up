import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Plug } from 'lucide-react';

export function IntegrationsSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Plug className="h-5 w-5" />
                        <span>Third-party Integrations</span>
                    </CardTitle>
                    <CardDescription>
                        Connect with external services and platforms
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-4">
                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">QuickBooks</div>
                                <Badge variant="outline">Accounting</Badge>
                            </div>
                            <Button variant="outline" size="sm">Connect</Button>
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Shopify</div>
                                <Badge variant="outline">E-commerce</Badge>
                            </div>
                            <Button variant="outline" size="sm">Connect</Button>
                        </div>

                        <div className="flex items-center justify-between">
                            <div className="flex items-center space-x-3">
                                <div className="font-medium">Mailchimp</div>
                                <Badge variant="outline">Email Marketing</Badge>
                            </div>
                            <Button variant="outline" size="sm">Connect</Button>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 