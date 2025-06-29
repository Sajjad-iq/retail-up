import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
    Settings,
    Store,
    ShoppingCart,
    Package,
    CreditCard,
    Users,
    UserCheck,
    Bell,
    BarChart3,
    Palette,
    Plug,
    HardDrive
} from 'lucide-react';

/**
 * Main settings overview page component
 * Displays all available settings categories for the retail management system
 */
export function SettingsPage() {
    const settingsCategories = [
        {
            title: 'Business & Store',
            icon: Store,
            description: 'Configure store information, business hours, and regional settings',
            features: ['Store details', 'Operating hours', 'Currency & timezone', 'Contact information'],
            color: 'bg-blue-500'
        },
        {
            title: 'Point of Sale',
            icon: ShoppingCart,
            description: 'POS interface settings, receipt configuration, and hardware setup',
            features: ['Receipt settings', 'Hardware devices', 'Interface preferences', 'Barcode scanning'],
            color: 'bg-green-500'
        },
        {
            title: 'Inventory Management',
            icon: Package,
            description: 'Stock tracking, product settings, and supplier management',
            features: ['Stock alerts', 'Product management', 'Supplier settings', 'Serial tracking'],
            color: 'bg-amber-500'
        },
        {
            title: 'Payments & Tax',
            icon: CreditCard,
            description: 'Payment methods, tax rates, and financial configuration',
            features: ['Payment methods', 'Tax settings', 'Financial policies', 'Currency handling'],
            color: 'bg-emerald-500'
        },
        {
            title: 'Staff & Security',
            icon: Users,
            description: 'User management, roles, permissions, and security policies',
            features: ['User roles', 'Security policies', 'Access control', 'Audit logging'],
            color: 'bg-purple-500'
        },
        {
            title: 'Customer Management',
            icon: UserCheck,
            description: 'Customer data collection, loyalty programs, and marketing',
            features: ['Customer data', 'Loyalty programs', 'Marketing preferences', 'Privacy settings'],
            color: 'bg-pink-500'
        },
        {
            title: 'Notifications',
            icon: Bell,
            description: 'Alert preferences, notification delivery, and communication settings',
            features: ['Email alerts', 'Push notifications', 'SMS settings', 'Quiet hours'],
            color: 'bg-orange-500'
        },
        {
            title: 'Reports & Analytics',
            icon: BarChart3,
            description: 'Report generation, analytics configuration, and export options',
            features: ['Auto reports', 'Analytics tracking', 'Export formats', 'Data insights'],
            color: 'bg-indigo-500'
        },
        {
            title: 'Appearance',
            icon: Palette,
            description: 'Theme, colors, layout, and display preferences',
            features: ['Theme selection', 'Color schemes', 'Layout options', 'Accessibility'],
            color: 'bg-violet-500'
        },
        {
            title: 'Integrations',
            icon: Plug,
            description: 'Third-party services, APIs, and external platform connections',
            features: ['Accounting software', 'E-commerce platforms', 'Marketing tools', 'Payment processors'],
            color: 'bg-cyan-500'
        },
        {
            title: 'System & Backup',
            icon: HardDrive,
            description: 'System configuration, data backup, and maintenance settings',
            features: ['Automatic backups', 'Data retention', 'System maintenance', 'Performance tuning'],
            color: 'bg-gray-500'
        }
    ];

    return (
        <div className="p-6">
            <div className="mb-8">
                <div className="flex items-center space-x-3 mb-4">
                    <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-primary text-primary-foreground">
                        <Settings className="h-6 w-6" />
                    </div>
                    <div>
                        <h1 className="text-3xl font-bold tracking-tight">Settings Overview</h1>
                        <p className="text-muted-foreground">
                            Configure your retail management system with comprehensive settings
                        </p>
                    </div>
                </div>

                <div className="flex items-center space-x-2">
                    <Badge variant="secondary">{settingsCategories.length} Categories</Badge>
                    <Badge variant="outline">Production Ready</Badge>
                </div>
            </div>

            <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
                {settingsCategories.map((category, index) => {
                    const Icon = category.icon;

                    return (
                        <Card key={index} className="relative overflow-hidden transition-all hover:shadow-md">
                            <CardHeader className="pb-3">
                                <div className="flex items-start justify-between">
                                    <div className={`flex h-10 w-10 items-center justify-center rounded-lg ${category.color} text-white`}>
                                        <Icon className="h-5 w-5" />
                                    </div>
                                    <Badge variant="outline" className="text-xs">
                                        Available
                                    </Badge>
                                </div>
                                <CardTitle className="text-lg">{category.title}</CardTitle>
                                <CardDescription className="text-sm">
                                    {category.description}
                                </CardDescription>
                            </CardHeader>
                            <CardContent>
                                <div className="space-y-2">
                                    <h4 className="text-sm font-medium text-muted-foreground">Key Features:</h4>
                                    <ul className="space-y-1">
                                        {category.features.map((feature, featureIndex) => (
                                            <li key={featureIndex} className="flex items-center text-sm">
                                                <div className="mr-2 h-1.5 w-1.5 rounded-full bg-muted-foreground" />
                                                {feature}
                                            </li>
                                        ))}
                                    </ul>
                                </div>
                            </CardContent>
                        </Card>
                    );
                })}
            </div>

            <div className="mt-8 rounded-lg border bg-muted/50 p-6">
                <h2 className="text-lg font-semibold mb-2">Getting Started</h2>
                <p className="text-muted-foreground mb-4">
                    Navigate to the main Settings interface to configure your retail system.
                    Each category provides comprehensive options tailored for modern retail operations.
                </p>
                <div className="grid gap-4 md:grid-cols-3">
                    <div className="flex items-center space-x-2">
                        <div className="flex h-8 w-8 items-center justify-center rounded-full bg-blue-100 text-blue-600">
                            <span className="text-sm font-semibold">1</span>
                        </div>
                        <span className="text-sm">Configure business basics</span>
                    </div>
                    <div className="flex items-center space-x-2">
                        <div className="flex h-8 w-8 items-center justify-center rounded-full bg-green-100 text-green-600">
                            <span className="text-sm font-semibold">2</span>
                        </div>
                        <span className="text-sm">Set up POS and inventory</span>
                    </div>
                    <div className="flex items-center space-x-2">
                        <div className="flex h-8 w-8 items-center justify-center rounded-full bg-purple-100 text-purple-600">
                            <span className="text-sm font-semibold">3</span>
                        </div>
                        <span className="text-sm">Customize appearance and integrations</span>
                    </div>
                </div>
            </div>
        </div>
    );
} 