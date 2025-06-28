import React from 'react';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Eye, DollarSign, Clock, AlertTriangle } from 'lucide-react';
import { formatPrice, formatDate, getPaymentPlanStatusColor } from '../../lib/utils/payment-plan-utils';
import type { PaymentPlan } from '../../types/payment-plans';

interface PaymentPlanTableProps {
    /** Payment plans to display */
    paymentPlans: PaymentPlan[];
    /** Callback when viewing plan details */
    onViewDetails: (plan: PaymentPlan) => void;
    /** Callback when collecting payment */
    onCollectPayment: (plan: PaymentPlan) => void;
    /** Loading state */
    isLoading?: boolean;
}

/**
 * PaymentPlanTable Component
 * 
 * Displays payment plans in a table format with actions for viewing details and collecting payments.
 */
export function PaymentPlanTable({
    paymentPlans,
    onViewDetails,
    onCollectPayment,
    isLoading = false
}: PaymentPlanTableProps) {
    if (isLoading) {
        return (
            <Card>
                <CardContent className="p-8 text-center text-muted-foreground">
                    Loading payment plans...
                </CardContent>
            </Card>
        );
    }

    if (paymentPlans.length === 0) {
        return (
            <Card>
                <CardContent className="p-8 text-center text-muted-foreground">
                    <DollarSign className="h-8 w-8 mx-auto mb-2" />
                    <p>No payment plans found</p>
                </CardContent>
            </Card>
        );
    }

    return (
        <Card>
            <CardHeader>
                <CardTitle className="flex items-center gap-2">
                    <DollarSign className="h-5 w-5" />
                    Payment Plans ({paymentPlans.length})
                </CardTitle>
            </CardHeader>
            <CardContent>
                <Table>
                    <TableHeader>
                        <TableRow>
                            <TableHead>Customer</TableHead>
                            <TableHead>Total Amount</TableHead>
                            <TableHead>Monthly Payment</TableHead>
                            <TableHead>Status</TableHead>
                            <TableHead>Next Payment</TableHead>
                            <TableHead>Actions</TableHead>
                        </TableRow>
                    </TableHeader>
                    <TableBody>
                        {paymentPlans.map((plan) => {
                            const nextPayment = plan.scheduledPayments.find(
                                payment => payment.status === 'pending'
                            );
                            const overduePayments = plan.scheduledPayments.filter(
                                payment => payment.status === 'overdue'
                            );

                            return (
                                <TableRow key={plan.id}>
                                    <TableCell>
                                        <div>
                                            <p className="font-medium">{plan.customer.name}</p>
                                            <p className="text-sm text-muted-foreground">
                                                {plan.customer.phone || plan.customer.email}
                                            </p>
                                        </div>
                                    </TableCell>
                                    <TableCell>
                                        <div>
                                            <p className="font-medium">{formatPrice(plan.totalAmount)}</p>
                                            <p className="text-sm text-muted-foreground">
                                                Down: {formatPrice(plan.downPayment)}
                                            </p>
                                        </div>
                                    </TableCell>
                                    <TableCell>
                                        <div>
                                            <p className="font-medium">{formatPrice(plan.monthlyPayment)}</p>
                                            <p className="text-sm text-muted-foreground">
                                                {plan.planDurationMonths} months
                                            </p>
                                        </div>
                                    </TableCell>
                                    <TableCell>
                                        <Badge className={getPaymentPlanStatusColor(plan.status)}>
                                            {plan.status}
                                        </Badge>
                                        {overduePayments.length > 0 && (
                                            <div className="flex items-center gap-1 mt-1">
                                                <AlertTriangle className="h-3 w-3 text-red-500" />
                                                <span className="text-xs text-red-600">
                                                    {overduePayments.length} overdue
                                                </span>
                                            </div>
                                        )}
                                    </TableCell>
                                    <TableCell>
                                        {nextPayment ? (
                                            <div>
                                                <p className="font-medium">{formatPrice(nextPayment.amount)}</p>
                                                <p className="text-sm text-muted-foreground flex items-center gap-1">
                                                    <Clock className="h-3 w-3" />
                                                    {formatDate(nextPayment.dueDate)}
                                                </p>
                                            </div>
                                        ) : (
                                            <span className="text-muted-foreground">No pending payments</span>
                                        )}
                                    </TableCell>
                                    <TableCell>
                                        <div className="flex items-center gap-2">
                                            <Button
                                                variant="outline"
                                                size="sm"
                                                onClick={() => onViewDetails(plan)}
                                            >
                                                <Eye className="h-4 w-4" />
                                            </Button>
                                            {plan.status === 'active' && nextPayment && (
                                                <Button
                                                    variant="default"
                                                    size="sm"
                                                    onClick={() => onCollectPayment(plan)}
                                                >
                                                    Collect Payment
                                                </Button>
                                            )}
                                        </div>
                                    </TableCell>
                                </TableRow>
                            );
                        })}
                    </TableBody>
                </Table>
            </CardContent>
        </Card>
    );
} 