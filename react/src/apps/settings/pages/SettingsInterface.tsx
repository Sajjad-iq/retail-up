import { useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Switch } from '@/components/ui/switch';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Separator } from '@/components/ui/separator';
import { Textarea } from '@/components/ui/textarea';
import { Badge } from '@/components/ui/badge';
import { useSettings } from '../hooks/use-settings';
import {
    Settings,
    Store,
    ShoppingCart,
    Package,
    CreditCard,
    Users,
    Bell,
    Shield,
    BarChart3,
    Palette,
    Plug,
    HardDrive,
    Save,
    RotateCcw,
    Download,
    Upload,
    AlertCircle,
    Mail,
    Phone,
    MapPin,
    Clock,
    DollarSign,
    Printer,
    Scan,
    UserCheck,
    Receipt,
    Percent,
    Database,
    Cloud,
    Wifi
} from 'lucide-react';
import { BusinessSettingsPanel } from '../components/business';
import { POSSettingsPanel } from '../components/pos';
import { InventorySettingsPanel } from '../components/inventory';
import { PaymentsSettingsPanel } from '../components/payments';
import { StaffSettingsPanel } from '../components/staff';
import { CustomersSettingsPanel } from '../components/customers';
import { NotificationsSettingsPanel } from '../components/notifications';
import { ReportsSettingsPanel } from '../components/reports';
import { AppearanceSettingsPanel } from '../components/appearance';
import { IntegrationsSettingsPanel } from '../components/integrations';
import { SystemSettingsPanel } from '../components/system';
import { cn } from '@/lib/utils';

/**
 * Main settings interface component
 * Provides comprehensive application configuration options
 */
export function SettingsInterface() {
    const [activeTab, setActiveTab] = useState('business');
    const { settings, updateSettings, resetSettings, isDirty, loading } = useSettings();

    const settingsCategories = [
        {
            id: 'business',
            label: 'Business & Store',
            icon: Store,
            description: 'Store information, locations, and business settings',
            color: 'bg-blue-500'
        },
        {
            id: 'pos',
            label: 'Point of Sale',
            icon: ShoppingCart,
            description: 'POS interface, receipts, and hardware settings',
            color: 'bg-green-500'
        },
        {
            id: 'inventory',
            label: 'Inventory',
            icon: Package,
            description: 'Stock management, alerts, and tracking settings',
            color: 'bg-amber-500'
        },
        {
            id: 'payments',
            label: 'Payments & Tax',
            icon: CreditCard,
            description: 'Payment methods, tax rates, and financial settings',
            color: 'bg-emerald-500'
        },
        {
            id: 'staff',
            label: 'Staff & Security',
            icon: Users,
            description: 'User management, roles, and security policies',
            color: 'bg-purple-500'
        },
        {
            id: 'customers',
            label: 'Customers',
            icon: UserCheck,
            description: 'Customer management and loyalty programs',
            color: 'bg-pink-500'
        },
        {
            id: 'notifications',
            label: 'Notifications',
            icon: Bell,
            description: 'Alert preferences and communication settings',
            color: 'bg-orange-500'
        },
        {
            id: 'reports',
            label: 'Reports & Analytics',
            icon: BarChart3,
            description: 'Reporting preferences and data analytics',
            color: 'bg-indigo-500'
        },
        {
            id: 'appearance',
            label: 'Appearance',
            icon: Palette,
            description: 'Theme, layout, and display preferences',
            color: 'bg-violet-500'
        },
        {
            id: 'integrations',
            label: 'Integrations',
            icon: Plug,
            description: 'Third-party services and API connections',
            color: 'bg-cyan-500'
        },
        {
            id: 'system',
            label: 'System & Backup',
            icon: HardDrive,
            description: 'System settings, backup, and data management',
            color: 'bg-gray-500'
        }
    ];

    const handleSaveChanges = async () => {
        try {
            await updateSettings(activeTab as any, settings[activeTab as keyof typeof settings]);
        } catch (error) {
            console.error('Failed to save settings:', error);
        }
    };

    const handleResetSettings = async () => {
        try {
            await resetSettings(activeTab as any);
        } catch (error) {
            console.error('Failed to reset settings:', error);
        }
    };

    const handleExportSettings = () => {
        const dataStr = JSON.stringify(settings, null, 2);
        const dataBlob = new Blob([dataStr], { type: 'application/json' });
        const url = URL.createObjectURL(dataBlob);
        const link = document.createElement('a');
        link.href = url;
        link.download = `retail-settings-${new Date().toISOString().split('T')[0]}.json`;
        link.click();
    };

    const handleImportSettings = () => {
        const input = document.createElement('input');
        input.type = 'file';
        input.accept = '.json';
        input.onchange = (e) => {
            const file = (e.target as HTMLInputElement).files?.[0];
            if (file) {
                const reader = new FileReader();
                reader.onload = (e) => {
                    try {
                        const importedSettings = JSON.parse(e.target?.result as string);
                        console.log('Imported settings:', importedSettings);
                    } catch (error) {
                        console.error('Failed to import settings:', error);
                    }
                };
                reader.readAsText(file);
            }
        };
        input.click();
    };

    return (
        <div className="h-full bg-background">
            <div className="flex h-full">
                {/* Sidebar */}
                <div className="w-80 border-r bg-muted/30">
                    <div className="flex h-full flex-col">
                        <div className="p-6">
                            <div className="space-y-4">
                                <div>
                                    <h2 className="text-lg font-semibold tracking-tight">Settings</h2>
                                    <p className="text-sm text-muted-foreground">
                                        Configure your retail application
                                    </p>
                                </div>

                                {isDirty && (
                                    <div className="flex items-center space-x-2 rounded-lg border border-orange-200 bg-orange-50 p-3">
                                        <AlertCircle className="h-4 w-4 text-orange-600" />
                                        <p className="text-sm text-orange-800">
                                            You have unsaved changes
                                        </p>
                                    </div>
                                )}

                                <div className="flex space-x-2">
                                    <Button
                                        size="sm"
                                        onClick={handleSaveChanges}
                                    >
                                        <Save className="mr-2 h-4 w-4" />
                                        Save Changes
                                    </Button>
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={handleResetSettings}
                                    >
                                        <RotateCcw className="mr-2 h-4 w-4" />
                                        Reset
                                    </Button>
                                </div>

                                <Separator className="my-4" />

                                <div className="flex space-x-2">
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={handleExportSettings}
                                        className="flex-1"
                                    >
                                        <Download className="mr-2 h-4 w-4" />
                                        Export
                                    </Button>
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={handleImportSettings}
                                        className="flex-1"
                                    >
                                        <Upload className="mr-2 h-4 w-4" />
                                        Import
                                    </Button>
                                </div>
                            </div>
                        </div>

                        <Separator />

                        {/* Navigation */}
                        <div className="flex-1 overflow-auto p-6">
                            <nav className="space-y-2">
                                {settingsCategories.map((category) => {
                                    const IconComponent = category.icon;
                                    return (
                                        <button
                                            key={category.id}
                                            onClick={() => setActiveTab(category.id)}
                                            className={cn(
                                                'w-full rounded-lg p-3 text-left transition-colors',
                                                'hover:bg-muted/50',
                                                activeTab === category.id
                                                    ? 'bg-primary text-primary-foreground'
                                                    : 'text-muted-foreground'
                                            )}
                                        >
                                            <div className="flex items-center space-x-3">
                                                <div className={cn(
                                                    'flex h-8 w-8 items-center justify-center rounded',
                                                    activeTab === category.id
                                                        ? 'bg-primary-foreground/20'
                                                        : category.color
                                                )}>
                                                    <IconComponent className="h-4 w-4" />
                                                </div>
                                                <div>
                                                    <div className="text-sm font-medium">{category.label}</div>
                                                    <div className="text-xs opacity-70">{category.description}</div>
                                                </div>
                                            </div>
                                        </button>
                                    );
                                })}
                            </nav>
                        </div>
                    </div>
                </div>

                {/* Main content */}
                <div className="flex-1">
                    <div className="flex h-full flex-col">
                        <div className="border-b p-6">
                            <div className="flex items-center justify-between">
                                <div>
                                    <h1 className="text-2xl font-semibold tracking-tight">
                                        {settingsCategories.find((cat) => cat.id === activeTab)?.label}
                                    </h1>
                                    <p className="text-muted-foreground">
                                        {settingsCategories.find((cat) => cat.id === activeTab)?.description}
                                    </p>
                                </div>
                                <div className="flex items-center space-x-2">
                                    <Settings className="h-5 w-5 text-muted-foreground" />
                                </div>
                            </div>
                        </div>

                        <div className="flex-1 overflow-auto p-6">
                            <div className="max-w-4xl">
                                {activeTab === 'business' && <BusinessSettingsPanel />}
                                {activeTab === 'pos' && <POSSettingsPanel />}
                                {activeTab === 'inventory' && <InventorySettingsPanel />}
                                {activeTab === 'payments' && <PaymentsSettingsPanel />}
                                {activeTab === 'staff' && <StaffSettingsPanel />}
                                {activeTab === 'customers' && <CustomersSettingsPanel />}
                                {activeTab === 'notifications' && <NotificationsSettingsPanel />}
                                {activeTab === 'reports' && <ReportsSettingsPanel />}
                                {activeTab === 'appearance' && <AppearanceSettingsPanel />}
                                {activeTab === 'integrations' && <IntegrationsSettingsPanel />}
                                {activeTab === 'system' && <SystemSettingsPanel />}
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
} 