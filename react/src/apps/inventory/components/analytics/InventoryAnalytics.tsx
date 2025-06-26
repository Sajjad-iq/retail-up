import React from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
    BarChart3,
    TrendingUp,
    TrendingDown,
    Package,
    DollarSign,
    Target,
    AlertTriangle,
    Star,
    Activity
} from 'lucide-react';

import { useInventoryAnalytics, useInventoryFormatters } from '../../hooks/use-inventory';

/**
 * InventoryAnalytics Component
 * 
 * Displays inventory analytics including charts, reports, and key performance indicators.
 * Provides insights into inventory performance, trends, and optimization opportunities.
 */
export function InventoryAnalytics() {
    const {
        totalInventoryValue,
        totalRetailValue,
        totalPotentialProfit,
        profitMargin,
        totalItems,
        stockStatusBreakdown,
        categoryBreakdown,
        movementsByType,
        topItemsByValue,
        recentMovementsCount
    } = useInventoryAnalytics();

    const { formatCurrency } = useInventoryFormatters();

    const getStockStatusColor = (status: string) => {
        switch (status) {
            case 'normal':
                return 'text-green-500';
            case 'low':
                return 'text-orange-500';
            case 'out':
                return 'text-red-500';
            case 'overstocked':
                return 'text-blue-500';
            default:
                return 'text-muted-foreground';
        }
    };

    const getStockStatusIcon = (status: string) => {
        switch (status) {
            case 'normal':
                return <Package className="h-4 w-4 text-green-500" />;
            case 'low':
                return <TrendingDown className="h-4 w-4 text-orange-500" />;
            case 'out':
                return <AlertTriangle className="h-4 w-4 text-red-500" />;
            case 'overstocked':
                return <TrendingUp className="h-4 w-4 text-blue-500" />;
            default:
                return <Package className="h-4 w-4" />;
        }
    };

    const getStockStatusLabel = (status: string) => {
        switch (status) {
            case 'normal':
                return 'Normal Stock';
            case 'low':
                return 'Low Stock';
            case 'out':
                return 'Out of Stock';
            case 'overstocked':
                return 'Overstocked';
            default:
                return status;
        }
    };

    return (
        <div className="h-full overflow-y-auto p-4 space-y-6">
            {/* Header */}
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-2xl font-semibold">Inventory Analytics</h2>
                    <p className="text-muted-foreground">
                        Comprehensive insights into your inventory performance
                    </p>
                </div>
                <Badge variant="outline">Last 30 days</Badge>
            </div>

            {/* Key Metrics */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Total Items</CardTitle>
                        <Package className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{totalItems}</div>
                        <p className="text-xs text-muted-foreground">
                            Active inventory items
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Inventory Value</CardTitle>
                        <DollarSign className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{totalInventoryValue}</div>
                        <p className="text-xs text-muted-foreground">
                            Current cost value
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Retail Value</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{totalRetailValue}</div>
                        <p className="text-xs text-muted-foreground">
                            Potential selling value
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Profit Margin</CardTitle>
                        <Target className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{profitMargin}</div>
                        <p className="text-xs text-muted-foreground">
                            Potential profit margin
                        </p>
                    </CardContent>
                </Card>
            </div>

            {/* Stock Status Breakdown */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <BarChart3 className="h-5 w-5" />
                        Stock Status Breakdown
                    </CardTitle>
                    <CardDescription>
                        Distribution of items by stock level status
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                        {Object.entries(stockStatusBreakdown).map(([status, count]) => (
                            <div key={status} className="flex items-center justify-between p-3 bg-muted/50 rounded-lg">
                                <div className="flex items-center gap-2">
                                    {getStockStatusIcon(status)}
                                    <span className="text-sm font-medium">
                                        {getStockStatusLabel(status)}
                                    </span>
                                </div>
                                <span className={`text-lg font-bold ${getStockStatusColor(status)}`}>
                                    {count}
                                </span>
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>

            {/* Category and Movement Analysis */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* Category Breakdown */}
                <Card>
                    <CardHeader>
                        <CardTitle>Category Breakdown</CardTitle>
                        <CardDescription>
                            Items and value by category
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-3">
                            {categoryBreakdown.length === 0 ? (
                                <p className="text-sm text-muted-foreground">No categories found</p>
                            ) : (
                                categoryBreakdown.map((category) => (
                                    <div key={category.category} className="flex items-center justify-between">
                                        <div>
                                            <p className="font-medium">{category.category}</p>
                                            <p className="text-sm text-muted-foreground">
                                                {category.itemCount} items
                                            </p>
                                        </div>
                                        <div className="text-right">
                                            <p className="font-semibold">
                                                {formatCurrency(category.totalValue)}
                                            </p>
                                            <p className="text-xs text-muted-foreground">
                                                {((category.totalValue / parseFloat(totalInventoryValue.replace(/[^0-9.-]+/g, ""))) * 100).toFixed(1)}%
                                            </p>
                                        </div>
                                    </div>
                                ))
                            )}
                        </div>
                    </CardContent>
                </Card>

                {/* Recent Movement Activity */}
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <Activity className="h-5 w-5" />
                            Movement Activity
                        </CardTitle>
                        <CardDescription>
                            Stock movements in the last 30 days
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-3">
                            <div className="flex items-center justify-between p-3 bg-muted/50 rounded-lg">
                                <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-green-500" />
                                    <span className="font-medium">Stock In</span>
                                </div>
                                <span className="text-lg font-bold text-green-600">
                                    {movementsByType.in || 0}
                                </span>
                            </div>
                            <div className="flex items-center justify-between p-3 bg-muted/50 rounded-lg">
                                <div className="flex items-center gap-2">
                                    <TrendingDown className="h-4 w-4 text-red-500" />
                                    <span className="font-medium">Stock Out</span>
                                </div>
                                <span className="text-lg font-bold text-red-600">
                                    {movementsByType.out || 0}
                                </span>
                            </div>
                            <div className="flex items-center justify-between p-3 bg-muted/50 rounded-lg">
                                <div className="flex items-center gap-2">
                                    <AlertTriangle className="h-4 w-4 text-blue-500" />
                                    <span className="font-medium">Adjustments</span>
                                </div>
                                <span className="text-lg font-bold text-blue-600">
                                    {movementsByType.adjustment || 0}
                                </span>
                            </div>
                            <div className="pt-2 border-t">
                                <p className="text-sm text-muted-foreground">
                                    Total movements: {recentMovementsCount}
                                </p>
                            </div>
                        </div>
                    </CardContent>
                </Card>
            </div>

            {/* Top Items by Value */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <Star className="h-5 w-5" />
                        Top Items by Value
                    </CardTitle>
                    <CardDescription>
                        Highest value inventory items based on current stock
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-3">
                        {topItemsByValue.length === 0 ? (
                            <p className="text-sm text-muted-foreground">No items found</p>
                        ) : (
                            topItemsByValue.map((item, index) => (
                                <div key={item.id} className="flex items-center justify-between p-3 bg-muted/50 rounded-lg">
                                    <div className="flex items-center gap-3">
                                        <div className="flex items-center justify-center w-8 h-8 bg-primary text-primary-foreground rounded-full text-sm font-bold">
                                            {index + 1}
                                        </div>
                                        <div>
                                            <p className="font-medium">{item.name}</p>
                                            <p className="text-sm text-muted-foreground">
                                                {item.sku} • Stock: {item.currentStock}
                                            </p>
                                        </div>
                                    </div>
                                    <div className="text-right">
                                        <p className="font-semibold">
                                            {formatCurrency(item.totalValue)}
                                        </p>
                                        <p className="text-xs text-muted-foreground">
                                            @{formatCurrency(item.sellingPrice)} each
                                        </p>
                                    </div>
                                </div>
                            ))
                        )}
                    </div>
                </CardContent>
            </Card>

            {/* Profitability Analysis */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <DollarSign className="h-5 w-5" />
                        Profitability Analysis
                    </CardTitle>
                    <CardDescription>
                        Potential profit and margin analysis
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <div className="text-center p-4 bg-muted/50 rounded-lg">
                            <p className="text-sm text-muted-foreground mb-1">Total Cost</p>
                            <p className="text-xl font-bold">{totalInventoryValue}</p>
                        </div>
                        <div className="text-center p-4 bg-muted/50 rounded-lg">
                            <p className="text-sm text-muted-foreground mb-1">Potential Revenue</p>
                            <p className="text-xl font-bold text-green-600">{totalRetailValue}</p>
                        </div>
                        <div className="text-center p-4 bg-muted/50 rounded-lg">
                            <p className="text-sm text-muted-foreground mb-1">Potential Profit</p>
                            <p className="text-xl font-bold text-blue-600">{totalPotentialProfit}</p>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 