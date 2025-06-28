import React from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Progress } from '@/components/ui/progress';
import {
    DollarSign,
    Calendar,
    TrendingUp,
    Users,
    AlertCircle,
    CheckCircle,
    Clock,
    XCircle
} from 'lucide-react';
import { formatPrice } from '../../lib/utils/payment-plan-utils';
import type { PaymentPlanAnalytics } from '../../types/payment-plans';

interface PaymentPlanAnalyticsProps {
    /** Analytics data */
    analytics: PaymentPlanAnalytics;
    /** Loading state */
    isLoading?: boolean;
}

/**
 * PaymentPlanAnalytics Component
 * 
 * Displays comprehensive analytics and metrics for payment plans.
 */
export function PaymentPlanAnalytics({
    analytics,
    isLoading = false
}: PaymentPlanAnalyticsProps) {
    if (isLoading) {
        return (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                {[...Array(8)].map((_, i) => (
                    <Card key={i}>
                        <CardContent className="p-6">
                            <div className="animate-pulse">
                                <div className="h-4 bg-gray-200 rounded w-3/4 mb-2"></div>
                                <div className="h-8 bg-gray-200 rounded w-1/2"></div>
                            </div>
                        </CardContent>
                    </Card>
                ))}
            </div>
        );
    }

    const totalPlans = analytics.activePlans + analytics.completedPlans + analytics.overduePlans;
    const completionRate = totalPlans > 0 ? (analytics.completedPlans / totalPlans) * 100 : 0;

    return (
        <div className="space-y-6">
            {/* Summary Cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                <Card>
                    <CardContent className="p-6">
                        <div className="flex items-center gap-2">
                            <Users className="h-4 w-4 text-blue-600" />
                            <div>
                                <p className="text-sm text-muted-foreground">Active Plans</p>
                                <p className="text-2xl font-bold text-blue-600">{analytics.activePlans}</p>
                            </div>
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardContent className="p-6">
                        <div className="flex items-center gap-2">
                            <CheckCircle className="h-4 w-4 text-green-600" />
                            <div>
                                <p className="text-sm text-muted-foreground">Completed Plans</p>
                                <p className="text-2xl font-bold text-green-600">{analytics.completedPlans}</p>
                            </div>
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardContent className="p-6">
                        <div className="flex items-center gap-2">
                            <AlertCircle className="h-4 w-4 text-red-600" />
                            <div>
                                <p className="text-sm text-muted-foreground">Overdue Plans</p>
                                <p className="text-2xl font-bold text-red-600">{analytics.overduePlans}</p>
                            </div>
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardContent className="p-6">
                        <div className="flex items-center gap-2">
                            <DollarSign className="h-4 w-4 text-green-600" />
                            <div>
                                <p className="text-sm text-muted-foreground">Total Revenue</p>
                                <p className="text-2xl font-bold text-green-600">
                                    {formatPrice(analytics.totalRevenue)}
                                </p>
                            </div>
                        </div>
                    </CardContent>
                </Card>
            </div>

            {/* Performance Metrics */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <TrendingUp className="h-5 w-5" />
                            Collection Performance
                        </CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div>
                            <div className="flex justify-between items-center mb-2">
                                <span className="text-sm text-muted-foreground">Monthly Collection Rate</span>
                                <span className="text-sm font-medium">
                                    {analytics.monthlyCollectionRate.toFixed(1)}%
                                </span>
                            </div>
                            <Progress
                                value={analytics.monthlyCollectionRate}
                                className="h-2"
                            />
                        </div>

                        <div>
                            <div className="flex justify-between items-center mb-2">
                                <span className="text-sm text-muted-foreground">Plan Completion Rate</span>
                                <span className="text-sm font-medium">
                                    {completionRate.toFixed(1)}%
                                </span>
                            </div>
                            <Progress
                                value={completionRate}
                                className="h-2"
                            />
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <Calendar className="h-5 w-5" />
                            Plan Metrics
                        </CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div className="flex justify-between items-center">
                            <span className="text-sm text-muted-foreground">Average Plan Duration</span>
                            <span className="text-sm font-medium">
                                {analytics.averagePlanDuration.toFixed(0)} months
                            </span>
                        </div>

                        <div className="flex justify-between items-center">
                            <span className="text-sm text-muted-foreground">Total Plans</span>
                            <span className="text-sm font-medium">{totalPlans}</span>
                        </div>

                        <div className="flex justify-between items-center">
                            <span className="text-sm text-muted-foreground">Average Revenue per Plan</span>
                            <span className="text-sm font-medium">
                                {totalPlans > 0
                                    ? formatPrice(analytics.totalRevenue / totalPlans)
                                    : formatPrice(0)
                                }
                            </span>
                        </div>
                    </CardContent>
                </Card>
            </div>

            {/* Status Distribution */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <Users className="h-5 w-5" />
                        Plan Status Distribution
                    </CardTitle>
                </CardHeader>
                <CardContent>
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <div className="flex items-center gap-3 p-4 bg-blue-50 rounded-lg">
                            <Clock className="h-8 w-8 text-blue-600" />
                            <div>
                                <p className="text-sm text-blue-700">Active Plans</p>
                                <p className="text-xl font-bold text-blue-900">{analytics.activePlans}</p>
                                <p className="text-xs text-blue-600">
                                    {totalPlans > 0 ? ((analytics.activePlans / totalPlans) * 100).toFixed(1) : 0}% of total
                                </p>
                            </div>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-green-50 rounded-lg">
                            <CheckCircle className="h-8 w-8 text-green-600" />
                            <div>
                                <p className="text-sm text-green-700">Completed Plans</p>
                                <p className="text-xl font-bold text-green-900">{analytics.completedPlans}</p>
                                <p className="text-xs text-green-600">
                                    {totalPlans > 0 ? ((analytics.completedPlans / totalPlans) * 100).toFixed(1) : 0}% of total
                                </p>
                            </div>
                        </div>

                        <div className="flex items-center gap-3 p-4 bg-red-50 rounded-lg">
                            <XCircle className="h-8 w-8 text-red-600" />
                            <div>
                                <p className="text-sm text-red-700">Overdue Plans</p>
                                <p className="text-xl font-bold text-red-900">{analytics.overduePlans}</p>
                                <p className="text-xs text-red-600">
                                    {totalPlans > 0 ? ((analytics.overduePlans / totalPlans) * 100).toFixed(1) : 0}% of total
                                </p>
                            </div>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
} 