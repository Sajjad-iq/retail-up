import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { TrendingUp, Plus, BarChart3, ArrowUpRight } from 'lucide-react';

/**
 * SalesAnalytics Component
 * 
 * Sales analytics and insights interface.
 */
export function SalesAnalytics() {
    return (
        <div className="p-6 space-y-6">
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-2xl font-bold">Sales Analytics</h2>
                    <p className="text-muted-foreground">
                        Comprehensive sales analytics with trends, forecasting, and performance metrics.
                    </p>
                </div>
                <Button>
                    <Plus className="mr-2 h-4 w-4" />
                    Generate Analytics
                </Button>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Total Sales</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">$89,400</div>
                        <div className="flex items-center gap-1 text-xs text-green-600">
                            <ArrowUpRight className="h-3 w-3" />
                            +15.3% from last month
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Transactions</CardTitle>
                        <BarChart3 className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">2,847</div>
                        <div className="flex items-center gap-1 text-xs text-green-600">
                            <ArrowUpRight className="h-3 w-3" />
                            +8.1% from last month
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Avg. Transaction</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">$67.50</div>
                        <div className="flex items-center gap-1 text-xs text-green-600">
                            <ArrowUpRight className="h-3 w-3" />
                            +6.7% from last month
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Conversion Rate</CardTitle>
                        <BarChart3 className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">3.2%</div>
                        <div className="flex items-center gap-1 text-xs text-green-600">
                            <ArrowUpRight className="h-3 w-3" />
                            +0.3% from last month
                        </div>
                    </CardContent>
                </Card>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <Card>
                    <CardHeader>
                        <CardTitle>Top Selling Products</CardTitle>
                        <CardDescription>
                            Best performing products this month
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            {[
                                { name: 'Samsung Galaxy S24', sales: '$12,500', units: '25 units', category: 'Electronics' },
                                { name: 'Nike Air Max', sales: '$8,750', units: '35 units', category: 'Clothing' },
                                { name: 'Coffee Maker Pro', sales: '$6,200', units: '31 units', category: 'Home' }
                            ].map((product, index) => (
                                <div key={index} className="flex items-center justify-between">
                                    <div className="space-y-1">
                                        <p className="text-sm font-medium">{product.name}</p>
                                        <div className="flex items-center gap-2">
                                            <Badge variant="outline" className="text-xs">
                                                {product.category}
                                            </Badge>
                                            <span className="text-xs text-muted-foreground">
                                                {product.units}
                                            </span>
                                        </div>
                                    </div>
                                    <div className="text-sm font-medium">
                                        {product.sales}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle>Sales by Category</CardTitle>
                        <CardDescription>
                            Revenue distribution across product categories
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            {[
                                { category: 'Electronics', revenue: '$35,000', percentage: 45, color: 'bg-blue-500' },
                                { category: 'Clothing', revenue: '$22,000', percentage: 28, color: 'bg-green-500' },
                                { category: 'Home & Garden', revenue: '$18,000', percentage: 23, color: 'bg-orange-500' },
                                { category: 'Books', revenue: '$3,200', percentage: 4, color: 'bg-purple-500' }
                            ].map((item, index) => (
                                <div key={index} className="space-y-2">
                                    <div className="flex items-center justify-between text-sm">
                                        <span className="font-medium">{item.category}</span>
                                        <span>{item.revenue}</span>
                                    </div>
                                    <div className="flex items-center gap-2">
                                        <div className="flex-1 bg-muted rounded-full h-2">
                                            <div
                                                className={`${item.color} h-2 rounded-full`}
                                                style={{ width: `${item.percentage}%` }}
                                            />
                                        </div>
                                        <span className="text-xs text-muted-foreground">
                                            {item.percentage}%
                                        </span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
} 