import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
    AlertTriangle,
    AlertCircle,
    Package,
    CheckCircle,
    RefreshCw,
    Filter,
    TrendingDown,
    X
} from 'lucide-react';

import { useInventoryAlerts, useInventoryFormatters } from '../../hooks/use-inventory';

/**
 * InventoryAlerts Component
 * 
 * Displays and manages inventory alerts including low stock and out of stock notifications.
 * Provides alert management and acknowledgment functionality.
 */
export function InventoryAlerts() {
    const [activeFilter, setActiveFilter] = useState<'all' | 'unacknowledged' | 'critical' | 'warning'>('all');

    const {
        alerts,
        totalAlerts,
        unacknowledgedCount,
        criticalCount,
        warningCount,
        lowStockItems,
        outOfStockItems,
        lowStockCount,
        outOfStockCount,
        acknowledgeAlert,
        refreshAlerts
    } = useInventoryAlerts();

    const { formatCurrency, formatDate } = useInventoryFormatters();

    // Filter alerts based on active filter
    const filteredAlerts = alerts.filter(alert => {
        switch (activeFilter) {
            case 'unacknowledged':
                return !alert.acknowledged;
            case 'critical':
                return alert.severity === 'critical';
            case 'warning':
                return alert.severity === 'warning';
            default:
                return true;
        }
    });

    const handleAcknowledgeAlert = (alertId: string) => {
        acknowledgeAlert(alertId);
    };

    const getSeverityIcon = (severity: 'warning' | 'critical') => {
        return severity === 'critical' ? (
            <AlertTriangle className="h-4 w-4 text-red-500" />
        ) : (
            <AlertCircle className="h-4 w-4 text-orange-500" />
        );
    };

    const getSeverityColor = (severity: 'warning' | 'critical') => {
        return severity === 'critical' ? 'destructive' : 'default';
    };

    return (
        <div className="h-full flex flex-col">
            {/* Header with Stats */}
            <div className="border-b bg-muted/20 p-4">
                <div className="flex items-center justify-between mb-4">
                    <div>
                        <h2 className="text-lg font-semibold">Inventory Alerts</h2>
                        <p className="text-sm text-muted-foreground">
                            Monitor and manage inventory alerts and notifications
                        </p>
                    </div>
                    <Button onClick={refreshAlerts} size="sm">
                        <RefreshCw className="mr-2 h-4 w-4" />
                        Refresh
                    </Button>
                </div>

                {/* Alert Stats */}
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <AlertTriangle className="h-4 w-4 text-red-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Critical</p>
                                    <p className="text-lg font-semibold">{criticalCount}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <AlertCircle className="h-4 w-4 text-orange-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Warning</p>
                                    <p className="text-lg font-semibold">{warningCount}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <Package className="h-4 w-4 text-blue-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Low Stock</p>
                                    <p className="text-lg font-semibold">{lowStockCount}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                    <Card>
                        <CardContent className="p-3">
                            <div className="flex items-center gap-2">
                                <X className="h-4 w-4 text-red-500" />
                                <div>
                                    <p className="text-xs text-muted-foreground">Out of Stock</p>
                                    <p className="text-lg font-semibold">{outOfStockCount}</p>
                                </div>
                            </div>
                        </CardContent>
                    </Card>
                </div>
            </div>

            {/* Content */}
            <div className="flex-1 overflow-hidden">
                <Tabs defaultValue="alerts" className="h-full flex flex-col">
                    <div className="border-b px-4 pt-4">
                        <TabsList>
                            <TabsTrigger value="alerts">
                                Alerts ({totalAlerts})
                            </TabsTrigger>
                            <TabsTrigger value="low-stock">
                                Low Stock ({lowStockCount})
                            </TabsTrigger>
                            <TabsTrigger value="out-of-stock">
                                Out of Stock ({outOfStockCount})
                            </TabsTrigger>
                        </TabsList>
                    </div>

                    <TabsContent value="alerts" className="flex-1 overflow-hidden">
                        <div className="h-full flex flex-col">
                            {/* Alert Filters */}
                            <div className="border-b bg-background p-4">
                                <div className="flex items-center gap-2">
                                    <Filter className="h-4 w-4 text-muted-foreground" />
                                    <div className="flex gap-2">
                                        <Button
                                            variant={activeFilter === 'all' ? 'default' : 'outline'}
                                            size="sm"
                                            onClick={() => setActiveFilter('all')}
                                        >
                                            All
                                        </Button>
                                        <Button
                                            variant={activeFilter === 'unacknowledged' ? 'default' : 'outline'}
                                            size="sm"
                                            onClick={() => setActiveFilter('unacknowledged')}
                                        >
                                            Unacknowledged ({unacknowledgedCount})
                                        </Button>
                                        <Button
                                            variant={activeFilter === 'critical' ? 'default' : 'outline'}
                                            size="sm"
                                            onClick={() => setActiveFilter('critical')}
                                        >
                                            Critical
                                        </Button>
                                        <Button
                                            variant={activeFilter === 'warning' ? 'default' : 'outline'}
                                            size="sm"
                                            onClick={() => setActiveFilter('warning')}
                                        >
                                            Warning
                                        </Button>
                                    </div>
                                </div>
                            </div>

                            {/* Alerts List */}
                            <div className="flex-1 overflow-y-auto p-4">
                                <div className="space-y-3">
                                    {filteredAlerts.length === 0 ? (
                                        <div className="text-center py-12">
                                            <CheckCircle className="h-12 w-12 mx-auto mb-4 text-green-500" />
                                            <h3 className="text-lg font-semibold mb-2">No alerts found</h3>
                                            <p className="text-muted-foreground">
                                                {activeFilter === 'all'
                                                    ? 'Everything looks good! No alerts to show.'
                                                    : `No ${activeFilter} alerts found.`
                                                }
                                            </p>
                                        </div>
                                    ) : (
                                        filteredAlerts.map((alert) => (
                                            <Card key={alert.id} className={`${alert.acknowledged ? 'opacity-60' : ''
                                                }`}>
                                                <CardContent className="p-4">
                                                    <div className="flex items-start justify-between">
                                                        <div className="flex items-start gap-3 flex-1">
                                                            {getSeverityIcon(alert.severity)}
                                                            <div className="flex-1">
                                                                <div className="flex items-center gap-2 mb-1">
                                                                    <h4 className="font-medium">
                                                                        {alert.inventoryItem.name}
                                                                    </h4>
                                                                    <Badge variant={getSeverityColor(alert.severity)}>
                                                                        {alert.severity}
                                                                    </Badge>
                                                                    {alert.acknowledged && (
                                                                        <Badge variant="outline">
                                                                            <CheckCircle className="mr-1 h-3 w-3" />
                                                                            Acknowledged
                                                                        </Badge>
                                                                    )}
                                                                </div>
                                                                <p className="text-sm text-muted-foreground mb-2">
                                                                    {alert.message}
                                                                </p>
                                                                <div className="flex items-center gap-4 text-xs text-muted-foreground">
                                                                    <span>SKU: {alert.inventoryItem.sku}</span>
                                                                    <span>Current Stock: {alert.inventoryItem.currentStock}</span>
                                                                    <span>Min Stock: {alert.inventoryItem.minimumStock}</span>
                                                                </div>
                                                                <p className="text-xs text-muted-foreground mt-1">
                                                                    Created: {formatDate(alert.createdAt)}
                                                                </p>
                                                            </div>
                                                        </div>
                                                        {!alert.acknowledged && (
                                                            <Button
                                                                size="sm"
                                                                variant="outline"
                                                                onClick={() => handleAcknowledgeAlert(alert.id)}
                                                            >
                                                                <CheckCircle className="mr-1 h-3 w-3" />
                                                                Acknowledge
                                                            </Button>
                                                        )}
                                                    </div>
                                                </CardContent>
                                            </Card>
                                        ))
                                    )}
                                </div>
                            </div>
                        </div>
                    </TabsContent>

                    <TabsContent value="low-stock" className="flex-1 overflow-hidden">
                        <div className="flex-1 overflow-y-auto p-4">
                            <div className="space-y-3">
                                {lowStockItems.length === 0 ? (
                                    <div className="text-center py-12">
                                        <CheckCircle className="h-12 w-12 mx-auto mb-4 text-green-500" />
                                        <h3 className="text-lg font-semibold mb-2">No low stock items</h3>
                                        <p className="text-muted-foreground">
                                            All items have sufficient stock levels.
                                        </p>
                                    </div>
                                ) : (
                                    lowStockItems.map((item) => (
                                        <Card key={item.id}>
                                            <CardContent className="p-4">
                                                <div className="flex items-center justify-between">
                                                    <div className="flex items-center gap-3">
                                                        <TrendingDown className="h-4 w-4 text-orange-500" />
                                                        <div>
                                                            <h4 className="font-medium">{item.name}</h4>
                                                            <p className="text-sm text-muted-foreground">
                                                                {item.sku} • {item.category}
                                                            </p>
                                                        </div>
                                                    </div>
                                                    <div className="text-right">
                                                        <div className="flex items-center gap-2">
                                                            <Badge variant="secondary">
                                                                Stock: {item.currentStock}
                                                            </Badge>
                                                            <Badge variant="outline">
                                                                Min: {item.minimumStock}
                                                            </Badge>
                                                        </div>
                                                        <p className="text-xs text-muted-foreground mt-1">
                                                            Reorder: {item.reorderQuantity} units
                                                        </p>
                                                    </div>
                                                </div>
                                            </CardContent>
                                        </Card>
                                    ))
                                )}
                            </div>
                        </div>
                    </TabsContent>

                    <TabsContent value="out-of-stock" className="flex-1 overflow-hidden">
                        <div className="flex-1 overflow-y-auto p-4">
                            <div className="space-y-3">
                                {outOfStockItems.length === 0 ? (
                                    <div className="text-center py-12">
                                        <CheckCircle className="h-12 w-12 mx-auto mb-4 text-green-500" />
                                        <h3 className="text-lg font-semibold mb-2">No out of stock items</h3>
                                        <p className="text-muted-foreground">
                                            All items are currently in stock.
                                        </p>
                                    </div>
                                ) : (
                                    outOfStockItems.map((item) => (
                                        <Card key={item.id}>
                                            <CardContent className="p-4">
                                                <div className="flex items-center justify-between">
                                                    <div className="flex items-center gap-3">
                                                        <X className="h-4 w-4 text-red-500" />
                                                        <div>
                                                            <h4 className="font-medium">{item.name}</h4>
                                                            <p className="text-sm text-muted-foreground">
                                                                {item.sku} • {item.category}
                                                            </p>
                                                        </div>
                                                    </div>
                                                    <div className="text-right">
                                                        <Badge variant="destructive">
                                                            Out of Stock
                                                        </Badge>
                                                        <p className="text-xs text-muted-foreground mt-1">
                                                            Cost: {formatCurrency(item.costPrice)}
                                                        </p>
                                                    </div>
                                                </div>
                                            </CardContent>
                                        </Card>
                                    ))
                                )}
                            </div>
                        </div>
                    </TabsContent>
                </Tabs>
            </div>
        </div>
    );
} 