import React, { useState } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Plus, Search, Filter, BarChart3, Calendar } from 'lucide-react';
import { usePaymentPlans } from '../hooks/use-payment-plans';
import { PaymentPlanTable } from '../components/management/PaymentPlanTable';
import { PaymentPlanAnalytics } from '../components/analytics/PaymentPlanAnalytics';
import type { PaymentPlan } from '../types/payment-plans';

/**
 * PaymentPlansInterface Component
 * 
 * Main interface for managing payment plans with tabs for different views.
 */
export function PaymentPlansInterface() {
    const [searchQuery, setSearchQuery] = useState('');
    const [statusFilter, setStatusFilter] = useState<'all' | PaymentPlan['status']>('all');
    const [selectedPlan, setSelectedPlan] = useState<PaymentPlan | null>(null);

    const {
        paymentPlans,
        isLoading,
        error,
        analytics,
        activePlans,
        searchByCustomer,
        filterByStatus,
        setSelectedPlan: setSelectedPlanInStore,
    } = usePaymentPlans();

    // Filter payment plans based on search and status
    const filteredPlans = React.useMemo(() => {
        let plans = paymentPlans;

        if (searchQuery.trim()) {
            plans = searchByCustomer(searchQuery);
        }

        if (statusFilter !== 'all') {
            plans = plans.filter(plan => plan.status === statusFilter);
        }

        return plans;
    }, [paymentPlans, searchQuery, statusFilter, searchByCustomer]);

    const handleViewDetails = (plan: PaymentPlan) => {
        setSelectedPlan(plan);
        setSelectedPlanInStore(plan);
        // TODO: Open details modal or navigate to details page
        console.log('View details for plan:', plan.id);
    };

    const handleCollectPayment = (plan: PaymentPlan) => {
        setSelectedPlan(plan);
        // TODO: Open payment collection dialog
        console.log('Collect payment for plan:', plan.id);
    };

    const handleCreatePaymentPlan = () => {
        // TODO: Open create payment plan dialog
        console.log('Create new payment plan');
    };

    if (error) {
        return (
            <Card className="m-6">
                <CardContent className="p-6 text-center text-red-600">
                    <p>Error loading payment plans: {error}</p>
                </CardContent>
            </Card>
        );
    }

    return (
        <div className="container mx-auto p-6 space-y-6">
            {/* Header */}
            <div className="flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold">Payment Plans</h1>
                    <p className="text-muted-foreground">
                        Manage customer payment plans and installments
                    </p>
                </div>
                <Button onClick={handleCreatePaymentPlan} className="flex items-center gap-2">
                    <Plus className="h-4 w-4" />
                    New Payment Plan
                </Button>
            </div>

            {/* Main Interface */}
            <Tabs defaultValue="overview" className="space-y-6">
                <TabsList className="grid w-full grid-cols-4">
                    <TabsTrigger value="overview" className="flex items-center gap-2">
                        <BarChart3 className="h-4 w-4" />
                        Overview
                    </TabsTrigger>
                    <TabsTrigger value="active" className="flex items-center gap-2">
                        <Calendar className="h-4 w-4" />
                        Active Plans
                    </TabsTrigger>
                    <TabsTrigger value="all" className="flex items-center gap-2">
                        <Filter className="h-4 w-4" />
                        All Plans
                    </TabsTrigger>
                    <TabsTrigger value="analytics" className="flex items-center gap-2">
                        <BarChart3 className="h-4 w-4" />
                        Analytics
                    </TabsTrigger>
                </TabsList>

                <TabsContent value="overview" className="space-y-6">
                    {/* Quick Stats */}
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Card>
                            <CardContent className="p-6">
                                <div className="flex items-center gap-2">
                                    <Calendar className="h-4 w-4 text-blue-600" />
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
                                    <BarChart3 className="h-4 w-4 text-green-600" />
                                    <div>
                                        <p className="text-sm text-muted-foreground">Collection Rate</p>
                                        <p className="text-2xl font-bold text-green-600">
                                            {analytics.monthlyCollectionRate.toFixed(1)}%
                                        </p>
                                    </div>
                                </div>
                            </CardContent>
                        </Card>

                        <Card>
                            <CardContent className="p-6">
                                <div className="flex items-center gap-2">
                                    <Filter className="h-4 w-4 text-orange-600" />
                                    <div>
                                        <p className="text-sm text-muted-foreground">Overdue Plans</p>
                                        <p className="text-2xl font-bold text-orange-600">{analytics.overduePlans}</p>
                                    </div>
                                </div>
                            </CardContent>
                        </Card>
                    </div>

                    {/* Recent Active Plans */}
                    <PaymentPlanTable
                        paymentPlans={activePlans.slice(0, 5)}
                        onViewDetails={handleViewDetails}
                        onCollectPayment={handleCollectPayment}
                        isLoading={isLoading}
                    />
                </TabsContent>

                <TabsContent value="active" className="space-y-6">
                    {/* Filters */}
                    <div className="flex gap-4">
                        <div className="flex-1">
                            <div className="relative">
                                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                                <Input
                                    placeholder="Search by customer name..."
                                    value={searchQuery}
                                    onChange={(e) => setSearchQuery(e.target.value)}
                                    className="pl-10"
                                />
                            </div>
                        </div>
                    </div>

                    <PaymentPlanTable
                        paymentPlans={searchQuery ? searchByCustomer(searchQuery).filter(p => p.status === 'active') : activePlans}
                        onViewDetails={handleViewDetails}
                        onCollectPayment={handleCollectPayment}
                        isLoading={isLoading}
                    />
                </TabsContent>

                <TabsContent value="all" className="space-y-6">
                    {/* Filters */}
                    <div className="flex gap-4">
                        <div className="flex-1">
                            <div className="relative">
                                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                                <Input
                                    placeholder="Search by customer name..."
                                    value={searchQuery}
                                    onChange={(e) => setSearchQuery(e.target.value)}
                                    className="pl-10"
                                />
                            </div>
                        </div>
                        <Select value={statusFilter} onValueChange={(value: any) => setStatusFilter(value)}>
                            <SelectTrigger className="w-48">
                                <SelectValue placeholder="Filter by status" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="all">All Status</SelectItem>
                                <SelectItem value="active">Active</SelectItem>
                                <SelectItem value="completed">Completed</SelectItem>
                                <SelectItem value="overdue">Overdue</SelectItem>
                                <SelectItem value="cancelled">Cancelled</SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <PaymentPlanTable
                        paymentPlans={filteredPlans}
                        onViewDetails={handleViewDetails}
                        onCollectPayment={handleCollectPayment}
                        isLoading={isLoading}
                    />
                </TabsContent>

                <TabsContent value="analytics" className="space-y-6">
                    <PaymentPlanAnalytics
                        analytics={analytics}
                        isLoading={isLoading}
                    />
                </TabsContent>
            </Tabs>
        </div>
    );
} 