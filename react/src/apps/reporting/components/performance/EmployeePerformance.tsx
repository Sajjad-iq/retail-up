import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback, AvatarImage } from '@/components/ui/avatar';
import { Users, Plus, TrendingUp, TrendingDown, Minus } from 'lucide-react';

/**
 * EmployeePerformance Component
 * 
 * Employee performance tracking and analytics interface.
 */
export function EmployeePerformance() {
    const employees = [
        {
            id: '1',
            name: 'John Smith',
            role: 'Sales Associate',
            avatar: '/avatars/john-smith.jpg',
            sales: 45000,
            transactions: 350,
            avgTransaction: 128.57,
            trend: 'up' as const,
            performance: 92
        },
        {
            id: '2',
            name: 'Sarah Johnson',
            role: 'Senior Sales Associate',
            avatar: '/avatars/sarah-johnson.jpg',
            sales: 52000,
            transactions: 285,
            avgTransaction: 182.46,
            trend: 'up' as const,
            performance: 96
        },
        {
            id: '3',
            name: 'Mike Chen',
            role: 'Sales Associate',
            avatar: '/avatars/mike-chen.jpg',
            sales: 38000,
            transactions: 420,
            avgTransaction: 90.48,
            trend: 'down' as const,
            performance: 78
        },
        {
            id: '4',
            name: 'Emma Wilson',
            role: 'Store Manager',
            avatar: '/avatars/emma-wilson.jpg',
            sales: 62000,
            transactions: 180,
            avgTransaction: 344.44,
            trend: 'stable' as const,
            performance: 88
        }
    ];

    const getTrendIcon = (trend: 'up' | 'down' | 'stable') => {
        switch (trend) {
            case 'up':
                return <TrendingUp className="h-4 w-4 text-green-600" />;
            case 'down':
                return <TrendingDown className="h-4 w-4 text-red-600" />;
            default:
                return <Minus className="h-4 w-4 text-gray-600" />;
        }
    };

    const getPerformanceBadge = (performance: number) => {
        if (performance >= 90) return <Badge className="bg-green-500">Excellent</Badge>;
        if (performance >= 80) return <Badge className="bg-blue-500">Good</Badge>;
        if (performance >= 70) return <Badge variant="secondary">Average</Badge>;
        return <Badge variant="destructive">Needs Improvement</Badge>;
    };

    return (
        <div className="p-6 space-y-6">
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-2xl font-bold">Employee Performance</h2>
                    <p className="text-muted-foreground">
                        Track and analyze employee performance metrics and sales achievements.
                    </p>
                </div>
                <Button>
                    <Plus className="mr-2 h-4 w-4" />
                    Performance Review
                </Button>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Total Employees</CardTitle>
                        <Users className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">24</div>
                        <p className="text-xs text-muted-foreground">
                            Active team members
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Avg Performance</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">88.5%</div>
                        <p className="text-xs text-muted-foreground">
                            Team average score
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Top Performer</CardTitle>
                        <Users className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">Sarah J.</div>
                        <p className="text-xs text-muted-foreground">
                            96% performance score
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Total Sales</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">$197k</div>
                        <p className="text-xs text-muted-foreground">
                            Combined team sales
                        </p>
                    </CardContent>
                </Card>
            </div>

            <Card>
                <CardHeader>
                    <CardTitle>Employee Performance Overview</CardTitle>
                    <CardDescription>
                        Individual performance metrics and trends
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-6">
                        {employees.map((employee) => (
                            <div key={employee.id} className="flex items-center justify-between p-4 border rounded-lg">
                                <div className="flex items-center gap-4">
                                    <Avatar>
                                        <AvatarImage src={employee.avatar} alt={employee.name} />
                                        <AvatarFallback>
                                            {employee.name.split(' ').map(n => n[0]).join('')}
                                        </AvatarFallback>
                                    </Avatar>
                                    <div>
                                        <div className="flex items-center gap-2">
                                            <h3 className="text-sm font-medium">{employee.name}</h3>
                                            {getTrendIcon(employee.trend)}
                                        </div>
                                        <p className="text-xs text-muted-foreground">{employee.role}</p>
                                        {getPerformanceBadge(employee.performance)}
                                    </div>
                                </div>

                                <div className="grid grid-cols-3 gap-6 text-right">
                                    <div>
                                        <p className="text-sm font-medium">${employee.sales.toLocaleString()}</p>
                                        <p className="text-xs text-muted-foreground">Total Sales</p>
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium">{employee.transactions}</p>
                                        <p className="text-xs text-muted-foreground">Transactions</p>
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium">${employee.avgTransaction.toFixed(2)}</p>
                                        <p className="text-xs text-muted-foreground">Avg Transaction</p>
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <Card>
                    <CardHeader>
                        <CardTitle>Performance Distribution</CardTitle>
                        <CardDescription>
                            Employee performance score distribution
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            {[
                                { range: '90-100%', count: 2, percentage: 50, color: 'bg-green-500' },
                                { range: '80-89%', count: 1, percentage: 25, color: 'bg-blue-500' },
                                { range: '70-79%', count: 1, percentage: 25, color: 'bg-yellow-500' },
                                { range: 'Below 70%', count: 0, percentage: 0, color: 'bg-red-500' }
                            ].map((item, index) => (
                                <div key={index} className="space-y-2">
                                    <div className="flex items-center justify-between text-sm">
                                        <span className="font-medium">{item.range}</span>
                                        <span>{item.count} employees</span>
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

                <Card>
                    <CardHeader>
                        <CardTitle>Performance Trends</CardTitle>
                        <CardDescription>
                            Employee improvement and decline trends
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-green-600" />
                                    <span className="text-sm">Improving Performance</span>
                                </div>
                                <span className="text-sm font-medium">2 employees</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <Minus className="h-4 w-4 text-gray-600" />
                                    <span className="text-sm">Stable Performance</span>
                                </div>
                                <span className="text-sm font-medium">1 employee</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <TrendingDown className="h-4 w-4 text-red-600" />
                                    <span className="text-sm">Declining Performance</span>
                                </div>
                                <span className="text-sm font-medium">1 employee</span>
                            </div>
                        </div>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
} 