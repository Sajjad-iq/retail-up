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
                            </div>
                        </div>

                        <div className="flex-1 overflow-auto px-6">
                            <div className="space-y-1 pb-6">
                                {settingsCategories.map((category) => {
                                    const Icon = category.icon;
                                    const isActive = activeTab === category.id;

                                    return (
                                        <button
                                            key={category.id}
                                            onClick={() => setActiveTab(category.id)}
                                            className={`w-full justify-start text-left ${isActive
                                                ? 'bg-secondary text-secondary-foreground'
                                                : 'hover:bg-secondary/50'
                                                } flex items-center space-x-3 rounded-md p-3 transition-colors`}
                                        >
                                            <div className={`flex h-8 w-8 items-center justify-center rounded-md ${category.color} text-white`}>
                                                <Icon className="h-4 w-4" />
                                            </div>
                                            <div className="flex-1 space-y-1">
                                                <p className="text-sm font-medium leading-none">
                                                    {category.label}
                                                </p>
                                                <p className="text-xs text-muted-foreground">
                                                    {category.description}
                                                </p>
                                            </div>
                                        </button>
                                    );
                                })}
                            </div>
                        </div>

                        <div className="border-t p-6">
                            <div className="space-y-2">
                                <Button
                                    variant="outline"
                                    size="sm"
                                    className="w-full justify-start"
                                    onClick={handleExportSettings}
                                >
                                    <Download className="mr-2 h-4 w-4" />
                                    Export Settings
                                </Button>
                                <Button
                                    variant="outline"
                                    size="sm"
                                    className="w-full justify-start"
                                    onClick={handleImportSettings}
                                >
                                    <Upload className="mr-2 h-4 w-4" />
                                    Import Settings
                                </Button>
                            </div>
                        </div>
                    </div>
                </div>

                {/* Main Content */}
                <div className="flex-1 flex flex-col">
                    <div className="border-b">
                        <div className="flex items-center justify-between p-6">
                            <div className="space-y-1">
                                <h1 className="text-2xl font-semibold tracking-tight">
                                    {settingsCategories.find(cat => cat.id === activeTab)?.label}
                                </h1>
                                <p className="text-sm text-muted-foreground">
                                    {settingsCategories.find(cat => cat.id === activeTab)?.description}
                                </p>
                            </div>
                            <div className="flex items-center space-x-2">
                                {isDirty && (
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={handleResetSettings}
                                        disabled={loading.saving}
                                    >
                                        <RotateCcw className="mr-2 h-4 w-4" />
                                        Reset
                                    </Button>
                                )}
                                <Button
                                    onClick={handleSaveChanges}
                                    disabled={!isDirty || loading.saving}
                                    size="sm"
                                >
                                    <Save className="mr-2 h-4 w-4" />
                                    {loading.saving ? 'Saving...' : 'Save Changes'}
                                </Button>
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
    );
}

function BusinessSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Store className="h-5 w-5" />
                        <span>Store Information</span>
                    </CardTitle>
                    <CardDescription>
                        Basic information about your business and store locations
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-6">
                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="businessName">Business Name</Label>
                            <Input id="businessName" placeholder="Your Business Name" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="storeName">Store Name</Label>
                            <Input id="storeName" placeholder="Main Store" />
                        </div>
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="address">Business Address</Label>
                        <Textarea
                            id="address"
                            placeholder="123 Main Street&#10;City, State 12345&#10;Country"
                            className="min-h-[80px]"
                        />
                    </div>

                    <div className="grid gap-4 md:grid-cols-3">
                        <div className="space-y-2">
                            <Label htmlFor="phone">Phone Number</Label>
                            <Input id="phone" placeholder="+1 (555) 123-4567" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="email">Email Address</Label>
                            <Input id="email" type="email" placeholder="info@store.com" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="website">Website</Label>
                            <Input id="website" placeholder="https://store.com" />
                        </div>
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Clock className="h-5 w-5" />
                        <span>Business Hours</span>
                    </CardTitle>
                    <CardDescription>
                        Set your regular operating hours for each day of the week
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-4">
                        {['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'].map((day) => (
                            <div key={day} className="flex items-center space-x-4">
                                <div className="w-20">
                                    <Label className="text-sm font-medium">{day}</Label>
                                </div>
                                <Switch />
                                <div className="flex items-center space-x-2">
                                    <Input className="w-24" placeholder="09:00" />
                                    <span className="text-sm text-muted-foreground">to</span>
                                    <Input className="w-24" placeholder="17:00" />
                                </div>
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <MapPin className="h-5 w-5" />
                        <span>Regional Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure currency, timezone, and regional preferences
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="grid gap-4 md:grid-cols-3">
                        <div className="space-y-2">
                            <Label htmlFor="currency">Currency</Label>
                            <Select>
                                <SelectTrigger>
                                    <SelectValue placeholder="Select currency" />
                                </SelectTrigger>
                                <SelectContent>
                                    <SelectItem value="USD">USD - US Dollar</SelectItem>
                                    <SelectItem value="EUR">EUR - Euro</SelectItem>
                                    <SelectItem value="GBP">GBP - British Pound</SelectItem>
                                    <SelectItem value="CAD">CAD - Canadian Dollar</SelectItem>
                                </SelectContent>
                            </Select>
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="timezone">Timezone</Label>
                            <Select>
                                <SelectTrigger>
                                    <SelectValue placeholder="Select timezone" />
                                </SelectTrigger>
                                <SelectContent>
                                    <SelectItem value="America/New_York">Eastern Time</SelectItem>
                                    <SelectItem value="America/Chicago">Central Time</SelectItem>
                                    <SelectItem value="America/Denver">Mountain Time</SelectItem>
                                    <SelectItem value="America/Los_Angeles">Pacific Time</SelectItem>
                                </SelectContent>
                            </Select>
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="language">Language</Label>
                            <Select>
                                <SelectTrigger>
                                    <SelectValue placeholder="Select language" />
                                </SelectTrigger>
                                <SelectContent>
                                    <SelectItem value="en">English</SelectItem>
                                    <SelectItem value="es">Spanish</SelectItem>
                                    <SelectItem value="fr">French</SelectItem>
                                </SelectContent>
                            </Select>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}

function POSSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Receipt className="h-5 w-5" />
                        <span>Receipt Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure receipt appearance and printing options
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Print receipts automatically</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically print receipts after each transaction
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Email receipts</Label>
                            <div className="text-sm text-muted-foreground">
                                Offer to email receipts to customers
                            </div>
                        </div>
                        <Switch />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="receiptFooter">Receipt Footer Text</Label>
                        <Textarea
                            id="receiptFooter"
                            placeholder="Thank you for your business!&#10;Visit us again soon."
                            className="min-h-[60px]"
                        />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="receiptLogo">Store Logo URL</Label>
                        <Input id="receiptLogo" placeholder="https://example.com/logo.png" />
                    </div>
                </CardContent>
            </Card>

            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Scan className="h-5 w-5" />
                        <span>Hardware Settings</span>
                    </CardTitle>
                    <CardDescription>
                        Configure barcode scanners, cash drawers, and other hardware
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Barcode scanner enabled</Label>
                            <div className="text-sm text-muted-foreground">
                                Enable barcode scanning for products
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Cash drawer connected</Label>
                            <div className="text-sm text-muted-foreground">
                                Automatically open cash drawer on cash sales
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Customer display</Label>
                            <div className="text-sm text-muted-foreground">
                                Show transaction details on customer-facing display
                            </div>
                        </div>
                        <Switch />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}

function InventorySettingsPanel() {
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

function PaymentsSettingsPanel() {
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

function StaffSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Users className="h-5 w-5" />
                        <span>User Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure user roles, permissions, and access control
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Require employee PIN</Label>
                            <div className="text-sm text-muted-foreground">
                                Staff must enter PIN to access POS
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Track employee sales</Label>
                            <div className="text-sm text-muted-foreground">
                                Record which employee processed each sale
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="sessionTimeout">Session timeout (minutes)</Label>
                        <Input id="sessionTimeout" type="number" placeholder="30" />
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
                        Configure security policies and audit settings
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Enable audit logging</Label>
                            <div className="text-sm text-muted-foreground">
                                Log all user actions and system changes
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Two-factor authentication</Label>
                            <div className="text-sm text-muted-foreground">
                                Require 2FA for admin accounts
                            </div>
                        </div>
                        <Switch />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}

function CustomersSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <UserCheck className="h-5 w-5" />
                        <span>Customer Management</span>
                    </CardTitle>
                    <CardDescription>
                        Configure customer data collection and management
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
                            <Label className="text-base">Enable loyalty program</Label>
                            <div className="text-sm text-muted-foreground">
                                Allow customers to earn and redeem points
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="grid gap-4 md:grid-cols-2">
                        <div className="space-y-2">
                            <Label htmlFor="pointsPerDollar">Points per dollar spent</Label>
                            <Input id="pointsPerDollar" type="number" placeholder="1" />
                        </div>
                        <div className="space-y-2">
                            <Label htmlFor="dollarPerPoint">Dollar value per point</Label>
                            <Input id="dollarPerPoint" type="number" step="0.01" placeholder="0.01" />
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}

function NotificationsSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Bell className="h-5 w-5" />
                        <span>Alert Preferences</span>
                    </CardTitle>
                    <CardDescription>
                        Configure when and how you receive notifications
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Email notifications</Label>
                            <div className="text-sm text-muted-foreground">
                                Receive notifications via email
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Push notifications</Label>
                            <div className="text-sm text-muted-foreground">
                                Browser push notifications
                            </div>
                        </div>
                        <Switch defaultChecked />
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Low stock alerts</Label>
                            <div className="text-sm text-muted-foreground">
                                Get notified when inventory is low
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
                </CardContent>
            </Card>
        </div>
    );
}

function ReportsSettingsPanel() {
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

function AppearanceSettingsPanel() {
    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                        <Palette className="h-5 w-5" />
                        <span>Theme & Display</span>
                    </CardTitle>
                    <CardDescription>
                        Customize the appearance and layout of your application
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-2">
                        <Label htmlFor="theme">Theme</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select theme" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="light">Light</SelectItem>
                                <SelectItem value="dark">Dark</SelectItem>
                                <SelectItem value="system">System</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="space-y-2">
                        <Label htmlFor="fontSize">Font Size</Label>
                        <Select>
                            <SelectTrigger>
                                <SelectValue placeholder="Select font size" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="small">Small</SelectItem>
                                <SelectItem value="medium">Medium</SelectItem>
                                <SelectItem value="large">Large</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="flex items-center justify-between">
                        <div className="space-y-0.5">
                            <Label className="text-base">Compact mode</Label>
                            <div className="text-sm text-muted-foreground">
                                Use a more compact layout to fit more content
                            </div>
                        </div>
                        <Switch />
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}

function IntegrationsSettingsPanel() {
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

function SystemSettingsPanel() {
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