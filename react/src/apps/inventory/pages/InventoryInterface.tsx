import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
    Package,
    Search,
    Plus,
    AlertTriangle,
    BarChart3,
    Settings,
    User,
    TrendingUp,
    TrendingDown,
    Package2,
    AlertCircle
} from 'lucide-react';

import { InventoryGrid } from '../components/inventory/InventoryGrid';
import { StockMovements } from '../components/movements/StockMovements';
import { InventoryAlerts } from '../components/alerts/InventoryAlerts';
import { InventoryAnalytics } from '../components/analytics/InventoryAnalytics';
import { AddItemDialog } from '../components/forms/AddItemDialog';
import { useInventoryItems, useInventoryAlerts, useInventoryAnalytics } from '../hooks/use-inventory';

/**
 * InventoryInterface Component
 * 
 * Main inventory management interface following the POS design pattern.
 * Multi-tab layout with inventory grid, movements, alerts, and analytics.
 */
export function InventoryInterface() {
    const [searchQuery, setSearchQuery] = useState('');
    const [activeTab, setActiveTab] = useState('inventory');
    const [showAddItemDialog, setShowAddItemDialog] = useState(false);

    const { totalItems, formattedTotalValue, activeItems } = useInventoryItems();
    const { unacknowledgedCount, criticalCount, lowStockCount } = useInventoryAlerts();
    const { totalInventoryValue, profitMargin } = useInventoryAnalytics();

    const handleAddItem = () => {
        setShowAddItemDialog(true);
    };

    const handleItemAdded = () => {
        setShowAddItemDialog(false);
    };

    return (
        <div className="full grid grid-rows-[73px_1fr] bg-background">
            {/* Header */}
            <header className="border-b bg-card">
                <div className="flex items-center justify-between p-4">
                    <div className="flex items-center gap-3">
                        <div className="flex items-center gap-2">
                            <Package className="h-6 w-6 text-primary" />
                            <h1 className="text-xl font-bold">Inventory Management</h1>
                        </div>
                        <Badge variant="secondary" className="text-xs">v1.0.0</Badge>
                    </div>

                    {/* Search */}
                    <div className="hidden md:flex items-center gap-4 flex-1 max-w-md mx-8">
                        <div className="relative flex-1">
                            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                            <Input
                                placeholder="Search inventory..."
                                value={searchQuery}
                                onChange={(e) => setSearchQuery(e.target.value)}
                                className="pl-9"
                            />
                        </div>
                    </div>

                    <div className="flex items-center gap-4">
                        {/* Today's Stats */}
                        <div className="hidden lg:flex items-center gap-6">
                            <div className="text-center">
                                <p className="text-xs text-muted-foreground">Total Items</p>
                                <p className="text-sm font-semibold">{totalItems}</p>
                            </div>
                            <div className="text-center">
                                <p className="text-xs text-muted-foreground">Total Value</p>
                                <p className="text-sm font-semibold">{formattedTotalValue}</p>
                            </div>
                            <div className="text-center">
                                <p className="text-xs text-muted-foreground">Profit Margin</p>
                                <p className="text-sm font-semibold">{profitMargin}</p>
                            </div>
                        </div>

                        {/* Alert Badge */}
                        {unacknowledgedCount > 0 && (
                            <div className="relative">
                                <AlertTriangle className="h-5 w-5 text-orange-500" />
                                <Badge
                                    variant="destructive"
                                    className="absolute -top-2 -right-2 h-5 w-5 p-0 flex items-center justify-center text-xs"
                                >
                                    {unacknowledgedCount}
                                </Badge>
                            </div>
                        )}

                        {/* User Avatar */}
                        <Avatar className="h-8 w-8">
                            <AvatarFallback>
                                <User className="h-4 w-4" />
                            </AvatarFallback>
                        </Avatar>
                    </div>
                </div>
            </header>

            {/* Main Content */}
            <div className="flex h-full">
                {/* Main Content Area */}
                <div className="flex-1 flex flex-col">
                    <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
                        {/* Tab Navigation */}
                        <div className="border-b bg-muted/20 px-4 pt-4">
                            <TabsList className="grid w-full max-w-md grid-cols-4">
                                <TabsTrigger value="inventory" className="text-xs">
                                    <Package2 className="h-4 w-4 mr-1" />
                                    Inventory
                                </TabsTrigger>
                                <TabsTrigger value="movements" className="text-xs">
                                    <TrendingUp className="h-4 w-4 mr-1" />
                                    Movements
                                </TabsTrigger>
                                <TabsTrigger value="alerts" className="text-xs">
                                    <AlertCircle className="h-4 w-4 mr-1" />
                                    Alerts
                                    {unacknowledgedCount > 0 && (
                                        <Badge variant="destructive" className="ml-1 h-4 w-4 p-0 text-xs">
                                            {unacknowledgedCount}
                                        </Badge>
                                    )}
                                </TabsTrigger>
                                <TabsTrigger value="analytics" className="text-xs">
                                    <BarChart3 className="h-4 w-4 mr-1" />
                                    Analytics
                                </TabsTrigger>
                            </TabsList>
                        </div>

                        {/* Tab Content */}
                        <div className="flex-1 overflow-hidden">
                            <TabsContent value="inventory" className="h-full m-0">
                                <InventoryGrid searchQuery={searchQuery} />
                            </TabsContent>

                            <TabsContent value="movements" className="h-full m-0">
                                <StockMovements />
                            </TabsContent>

                            <TabsContent value="alerts" className="h-full m-0">
                                <InventoryAlerts />
                            </TabsContent>

                            <TabsContent value="analytics" className="h-full m-0">
                                <InventoryAnalytics />
                            </TabsContent>
                        </div>
                    </Tabs>
                </div>

                {/* Right Sidebar - Quick Actions & Stats */}
                <div className="w-80 border-l bg-muted/20 flex flex-col">
                    {/* Quick Actions */}
                    <div className="p-4 border-b">
                        <h3 className="text-sm font-semibold mb-3 flex items-center gap-2">
                            <Settings className="h-4 w-4" />
                            Quick Actions
                        </h3>
                        <div className="space-y-2">
                            <Button
                                className="w-full justify-start"
                                onClick={handleAddItem}
                                aria-label="Add new inventory item"
                            >
                                <Plus className="mr-2 h-4 w-4" />
                                Add New Item
                            </Button>
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                disabled
                                aria-label="Import items (coming soon)"
                            >
                                <Package className="mr-2 h-4 w-4" />
                                Import Items
                            </Button>
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                disabled
                                aria-label="Generate report (coming soon)"
                            >
                                <BarChart3 className="mr-2 h-4 w-4" />
                                Generate Report
                            </Button>
                        </div>
                    </div>

                    {/* Quick Stats */}
                    <div className="p-4 border-b">
                        <h3 className="text-sm font-semibold mb-3">Quick Stats</h3>
                        <div className="space-y-3">
                            <div className="flex items-center justify-between p-2 bg-background rounded border">
                                <div className="flex items-center gap-2">
                                    <Package2 className="h-4 w-4 text-blue-500" />
                                    <span className="text-sm">Active Items</span>
                                </div>
                                <span className="text-sm font-semibold">{activeItems.length}</span>
                            </div>

                            <div className="flex items-center justify-between p-2 bg-background rounded border">
                                <div className="flex items-center gap-2">
                                    <TrendingDown className="h-4 w-4 text-orange-500" />
                                    <span className="text-sm">Low Stock</span>
                                </div>
                                <span className="text-sm font-semibold">{lowStockCount}</span>
                            </div>

                            <div className="flex items-center justify-between p-2 bg-background rounded border">
                                <div className="flex items-center gap-2">
                                    <AlertTriangle className="h-4 w-4 text-red-500" />
                                    <span className="text-sm">Critical Alerts</span>
                                </div>
                                <span className="text-sm font-semibold">{criticalCount}</span>
                            </div>

                            <div className="flex items-center justify-between p-2 bg-background rounded border">
                                <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-green-500" />
                                    <span className="text-sm">Total Value</span>
                                </div>
                                <span className="text-sm font-semibold">{formattedTotalValue}</span>
                            </div>
                        </div>
                    </div>

                    {/* Recent Activity */}
                    <div className="flex-1 p-4 overflow-y-auto">
                        <h3 className="text-sm font-semibold mb-3">Recent Activity</h3>
                        <div className="space-y-2">
                            <div className="text-xs text-muted-foreground p-2 bg-background rounded border">
                                Recent activity will appear here...
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Add Item Dialog */}
            <AddItemDialog
                isOpen={showAddItemDialog}
                onClose={() => setShowAddItemDialog(false)}
                onSuccess={handleItemAdded}
            />
        </div>
    );
}