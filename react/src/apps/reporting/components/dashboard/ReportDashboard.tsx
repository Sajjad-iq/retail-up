import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
    DollarSign,
    TrendingUp,
    Users,
    BarChart3,
    ArrowUpRight,
    ArrowDownRight
} from 'lucide-react';

/**
 * ReportDashboard Component
 * 
 * Main dashboard overview for the reporting system showing key metrics and recent activity.
 */
export function ReportDashboard() {
    const metrics = [
        {
            title: 'Total Revenue',
            value: '$125,000',
            change: '+12.5%',
            trend: 'up' as const,
            icon: DollarSign,
            description: 'Revenue for this month'
        },
        {
            title: 'Net Profit',
            value: '$45,000',
            change: '+8.2%',
            trend: 'up' as const,
            icon: TrendingUp,
            description: 'Net profit margin'
        },
        {
            title: 'Total Sales',
            value: '2,847',
            change: '+15.3%',
            trend: 'up' as const,
            icon: BarChart3,
            description: 'Sales transactions'
        },
        {
            title: 'Active Employees',
            value: '24',
            change: '-2.1%',
            trend: 'down' as const,
            icon: Users,
            description: 'Current staff count'
        }
    ];

    const recentReports = [
        {
            title: 'Monthly Financial Report',
            type: 'Financial',
            date: '2 hours ago',
            status: 'completed'
        },
        {
            title: 'Sales Analytics Q4',
            type: 'Sales',
            date: '1 day ago',
            status: 'completed'
        },
        {
            title: 'Employee Performance Review',
            type: 'Performance',
            date: '3 days ago',
            status: 'completed'
        }
    ];

    return (
        <div className="p-6 space-y-6">
            {/* Header */}
            <div>
                <h1 className="text-3xl font-bold">Reporting Dashboard</h1>
                <p className="text-muted-foreground mt-2">
                    Comprehensive business intelligence and analytics overview
                </p>
            </div>

            {/* Key Metrics Grid */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                {metrics.map((metric) => {
                    const Icon = metric.icon;
                    const TrendIcon = metric.trend === 'up' ? ArrowUpRight : ArrowDownRight;
                    const trendColor = metric.trend === 'up' ? 'text-green-600' : 'text-red-600';

                    return (
                        <Card key={metric.title}>
                            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                                <CardTitle className="text-sm font-medium">
                                    {metric.title}
                                </CardTitle>
                                <Icon className="h-4 w-4 text-muted-foreground" />
                            </CardHeader>
                            <CardContent>
                                <div className="text-2xl font-bold">{metric.value}</div>
                                <div className="flex items-center gap-1 text-xs text-muted-foreground">
                                    <TrendIcon className={`h-3 w-3 ${trendColor}`} />
                                    <span className={trendColor}>{metric.change}</span>
                                    <span>from last month</span>
                                </div>
                                <p className="text-xs text-muted-foreground mt-1">
                                    {metric.description}
                                </p>
                            </CardContent>
                        </Card>
                    );
                })}
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* Recent Reports */}
                <Card>
                    <CardHeader>
                        <CardTitle>Recent Reports</CardTitle>
                        <CardDescription>
                            Latest generated reports and analytics
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            {recentReports.map((report, index) => (
                                <div key={index} className="flex items-center justify-between">
                                    <div className="space-y-1">
                                        <p className="text-sm font-medium">{report.title}</p>
                                        <div className="flex items-center gap-2">
                                            <Badge variant="outline" className="text-xs">
                                                {report.type}
                                            </Badge>
                                            <span className="text-xs text-muted-foreground">
                                                {report.date}
                                            </span>
                                        </div>
                                    </div>
                                    <Badge variant="secondary" className="text-xs">
                                        {report.status}
                                    </Badge>
                                </div>
                            ))}
                        </div>
                    </CardContent>
                </Card>

                {/* Quick Stats */}
                <Card>
                    <CardHeader>
                        <CardTitle>Quick Stats</CardTitle>
                        <CardDescription>
                            Key performance indicators at a glance
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            <div className="flex items-center justify-between">
                                <span className="text-sm">Average Transaction Value</span>
                                <span className="text-sm font-medium">$67.50</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <span className="text-sm">Customer Retention Rate</span>
                                <span className="text-sm font-medium">85.3%</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <span className="text-sm">Inventory Turnover</span>
                                <span className="text-sm font-medium">12.4x</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <span className="text-sm">Profit Margin</span>
                                <span className="text-sm font-medium">36%</span>
                            </div>
                        </div>
                    </CardContent>
                </Card>
            </div>

            {/* Additional Insights */}
            <Card>
                <CardHeader>
                    <CardTitle>Business Insights</CardTitle>
                    <CardDescription>
                        AI-powered recommendations and trends
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-4">
                        <div className="flex items-start gap-3">
                            <div className="w-2 h-2 bg-green-500 rounded-full mt-2"></div>
                            <div>
                                <p className="text-sm font-medium">Revenue Growth Trend</p>
                                <p className="text-xs text-muted-foreground">
                                    Revenue has increased by 12.5% compared to last month, indicating strong business performance.
                                </p>
                            </div>
                        </div>
                        <div className="flex items-start gap-3">
                            <div className="w-2 h-2 bg-blue-500 rounded-full mt-2"></div>
                            <div>
                                <p className="text-sm font-medium">Customer Acquisition</p>
                                <p className="text-xs text-muted-foreground">
                                    New customer acquisition is up 18% with improved retention rates.
                                </p>
                            </div>
                        </div>
                        <div className="flex items-start gap-3">
                            <div className="w-2 h-2 bg-orange-500 rounded-full mt-2"></div>
                            <div>
                                <p className="text-sm font-medium">Inventory Optimization</p>
                                <p className="text-xs text-muted-foreground">
                                    Consider restocking high-turnover items to maintain optimal inventory levels.
                                </p>
                            </div>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 