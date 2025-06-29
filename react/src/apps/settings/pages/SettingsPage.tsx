import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Settings } from 'lucide-react';

/**
 * Main settings page component
 * Provides comprehensive application configuration options
 */
export function SettingsPage() {
    return (
        <div className="p-6">
            <div className="mb-6">
                <h1 className="text-3xl font-bold flex items-center gap-2">
                    <Settings className="h-8 w-8" />
                    Application Settings
                </h1>
                <p className="text-muted-foreground">
                    Configure your application preferences and system settings
                </p>
            </div>

            <div className="grid gap-6">
                <Card>
                    <CardHeader>
                        <CardTitle>General Settings</CardTitle>
                        <CardDescription>
                            Basic application and store configuration
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <p className="text-muted-foreground">
                            General settings panel - Configure store information, business hours,
                            currency, timezone, and basic application preferences.
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle>Appearance</CardTitle>
                        <CardDescription>
                            Customize the look and feel of your application
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <p className="text-muted-foreground">
                            Appearance settings - Configure theme, colors, font sizes,
                            layout preferences, and display options.
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle>Notifications</CardTitle>
                        <CardDescription>
                            Configure notification preferences and delivery settings
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <p className="text-muted-foreground">
                            Notification settings - Configure email, push, SMS, and in-app
                            notification preferences, quiet hours, and alert frequencies.
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle>Security</CardTitle>
                        <CardDescription>
                            Manage security settings and data protection
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <p className="text-muted-foreground">
                            Security settings - Configure two-factor authentication, session management,
                            password policies, login attempts, and audit logging.
                        </p>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
} 