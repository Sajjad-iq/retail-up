import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { BarChart3, Users, TrendingUp, Shield, Activity } from 'lucide-react';

import { useAuthAnalytics } from '../../hooks/use-auth';

/**
 * AuthAnalytics Component
 * 
 * Displays authentication and user management analytics.
 * Shows user statistics, role distribution, and login trends.
 */
export function AuthAnalytics() {
    const analytics = useAuthAnalytics();

    return (
        <div className="h-full flex flex-col p-4 space-y-4">
            {/* Overview Cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Total Users</CardTitle>
                        <Users className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{analytics.totalUsers}</div>
                        <p className="text-xs text-muted-foreground">
                            {analytics.activeUsers} active
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Logins Today</CardTitle>
                        <TrendingUp className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{analytics.loginsToday}</div>
                        <p className="text-xs text-muted-foreground">
                            Unique user sessions
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">New This Month</CardTitle>
                        <Users className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{analytics.newUsersThisMonth}</div>
                        <p className="text-xs text-muted-foreground">
                            New user accounts
                        </p>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                        <CardTitle className="text-sm font-medium">Active Users</CardTitle>
                        <Activity className="h-4 w-4 text-muted-foreground" />
                    </CardHeader>
                    <CardContent>
                        <div className="text-2xl font-bold">{analytics.activeUsers}</div>
                        <p className="text-xs text-muted-foreground">
                            {Math.round((analytics.activeUsers / analytics.totalUsers) * 100)}% of total
                        </p>
                    </CardContent>
                </Card>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 flex-1">
                {/* Role Distribution */}
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <Shield className="h-5 w-5" />
                            Role Distribution
                        </CardTitle>
                        <CardDescription>
                            User count by role assignment
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            {analytics.roleDistribution.map((role, index) => (
                                <div key={role.role} className="space-y-2">
                                    <div className="flex items-center justify-between">
                                        <span className="text-sm font-medium">{role.role}</span>
                                        <div className="flex items-center gap-2">
                                            <span className="text-sm text-muted-foreground">{role.count} users</span>
                                            <Badge variant="outline" className="text-xs">
                                                {role.percentage}%
                                            </Badge>
                                        </div>
                                    </div>
                                    <Progress value={role.percentage} className="h-2" />
                                </div>
                            ))}
                        </div>
                    </CardContent>
                </Card>

                {/* Most Active Users */}
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <TrendingUp className="h-5 w-5" />
                            Most Active Users
                        </CardTitle>
                        <CardDescription>
                            Users with highest activity in last 30 days
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        {analytics.mostActiveUsers.length === 0 ? (
                            <div className="text-center py-8">
                                <Activity className="h-8 w-8 text-muted-foreground mx-auto mb-2" />
                                <p className="text-sm text-muted-foreground">No activity data available</p>
                            </div>
                        ) : (
                            <div className="space-y-3">
                                {analytics.mostActiveUsers.map((user, index) => (
                                    <div key={user.id} className="flex items-center justify-between p-2 border rounded">
                                        <div className="flex items-center gap-2">
                                            <Badge variant="outline" className="text-xs">
                                                #{index + 1}
                                            </Badge>
                                            <div>
                                                <div className="font-medium text-sm">{user.name}</div>
                                                <div className="text-xs text-muted-foreground">{user.role.name}</div>
                                            </div>
                                        </div>
                                        <div className="text-right">
                                            <div className="text-sm font-medium">{(user as any).activityCount}</div>
                                            <div className="text-xs text-muted-foreground">actions</div>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        )}
                    </CardContent>
                </Card>
            </div>

            {/* Login Activity Chart Placeholder */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <BarChart3 className="h-5 w-5" />
                        Login Activity (Last 7 Days)
                    </CardTitle>
                    <CardDescription>
                        Daily login activity trend
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="h-32 flex items-end justify-between gap-2">
                        {analytics.loginActivity.map((day, index) => (
                            <div key={day.date} className="flex flex-col items-center flex-1">
                                <div
                                    className="bg-primary rounded-t w-full min-h-[4px]"
                                    style={{
                                        height: `${Math.max(4, (day.count / Math.max(...analytics.loginActivity.map(d => d.count))) * 100)}px`
                                    }}
                                />
                                <div className="text-xs text-muted-foreground mt-2">
                                    {new Date(day.date).toLocaleDateString('en-US', { weekday: 'short' })}
                                </div>
                                <div className="text-xs font-medium">{day.count}</div>
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 