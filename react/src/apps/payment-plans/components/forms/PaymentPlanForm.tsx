import React, { useState, useEffect, useMemo } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Separator } from '@/components/ui/separator';
import { Calendar, DollarSign, Calculator, User } from 'lucide-react';
import {
    Form,
    FormControl,
    FormDescription,
    FormField,
    FormItem,
    FormLabel,
    FormMessage
} from '@/components/ui/form';
import { paymentPlanFormSchema, type PaymentPlanFormInput } from '../../lib/validations/payment-plan-schemas';
import { formatPrice } from '../../lib/utils/payment-plan-utils';
import type { Customer, PaymentPlan } from '../../types/payment-plans';

interface PaymentPlanFormProps {
    /** Total amount to be financed */
    totalAmount: number;
    /** Current customer information */
    customer: Customer | null;
    /** Callback when customer changes */
    onCustomerChange: (customer: Customer | null) => void;
    /** Callback when payment plan is configured */
    onPaymentPlanChange: (plan: PaymentPlan | null) => void;
}

/**
 * PaymentPlanForm Component
 * 
 * Handles payment plan setup where customers pay a down payment upfront and remaining amount monthly.
 */
export function PaymentPlanForm({
    totalAmount,
    customer,
    onCustomerChange,
    onPaymentPlanChange
}: PaymentPlanFormProps) {
    const [calculatedPlan, setCalculatedPlan] = useState<PaymentPlan | null>(null);

    const form = useForm<PaymentPlanFormInput>({
        resolver: zodResolver(paymentPlanFormSchema),
        defaultValues: {
            customer: customer || {
                id: '',
                name: '',
                email: '',
                phone: '',
            },
            downPayment: totalAmount * 0.5,
            totalAmount,
            planDurationMonths: 6,
            interestRate: 0,
        },
    });

    // Watch specific form fields to avoid unnecessary re-renders
    const downPayment = form.watch('downPayment');
    const planDurationMonths = form.watch('planDurationMonths');
    const interestRate = form.watch('interestRate');
    const customerData = form.watch('customer');

    /**
     * Calculate payment plan details when relevant form values change
     */
    const memoizedPlan = useMemo(() => {
        if (downPayment && planDurationMonths && downPayment < totalAmount && downPayment >= totalAmount * 0.1) {
            const remainingAmount = totalAmount - downPayment;
            const monthlyInterestRate = interestRate / 100 / 12;

            let monthlyPayment: number;

            if (interestRate > 0) {
                // Calculate monthly payment with interest using standard loan formula
                monthlyPayment = remainingAmount *
                    (monthlyInterestRate * Math.pow(1 + monthlyInterestRate, planDurationMonths)) /
                    (Math.pow(1 + monthlyInterestRate, planDurationMonths) - 1);
            } else {
                // Simple division for 0% interest
                monthlyPayment = remainingAmount / planDurationMonths;
            }

            const now = new Date();
            const scheduledPayments = Array.from({ length: planDurationMonths }, (_, index) => {
                const dueDate = new Date(now);
                dueDate.setMonth(dueDate.getMonth() + index + 1);

                return {
                    id: `payment-${index + 1}`,
                    planId: '',
                    dueDate,
                    amount: monthlyPayment,
                    status: 'pending' as const,
                };
            });

            const plan: PaymentPlan = {
                id: `plan-${Date.now()}`,
                transactionId: '',
                customer: customerData,
                totalAmount,
                downPayment,
                remainingAmount,
                monthlyPayment,
                planDurationMonths,
                interestRate,
                status: 'active',
                createdAt: now,
                startDate: now,
                scheduledPayments,
            };

            return plan;
        }
        return null;
    }, [downPayment, planDurationMonths, interestRate, customerData, totalAmount]);

    /**
     * Update parent component when calculated plan changes
     */
    useEffect(() => {
        setCalculatedPlan(memoizedPlan);
        onPaymentPlanChange(memoizedPlan);

        // Also update the customer in the parent when plan is valid
        if (memoizedPlan && memoizedPlan.customer.name.trim()) {
            onCustomerChange(memoizedPlan.customer);
        }
    }, [memoizedPlan, onPaymentPlanChange, onCustomerChange]);

    /**
     * Handle customer information updates - simplified to avoid re-render loops
     */
    const handleCustomerUpdate = (field: keyof Customer, value: string) => {
        const currentCustomer = form.getValues('customer');
        const updatedCustomer = {
            ...currentCustomer,
            [field]: value,
            id: currentCustomer.id || `customer-${Date.now()}`,
        };

        form.setValue('customer', updatedCustomer);
    };

    /**
     * Set predefined down payment percentage
     */
    const setDownPaymentPercentage = (percentage: number) => {
        const amount = totalAmount * percentage;
        form.setValue('downPayment', amount);
    };

    return (
        <div className="space-y-6">
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <Calendar className="h-5 w-5" />
                        Payment Plan Setup
                    </CardTitle>
                </CardHeader>
                <CardContent>
                    <Form {...form}>
                        <div className="space-y-6">
                            {/* Customer Information */}
                            <div className="space-y-4">
                                <div className="flex items-center gap-2">
                                    <User className="h-4 w-4" />
                                    <h4 className="font-medium">Customer Information</h4>
                                </div>
                                <div className="grid grid-cols-2 gap-4">
                                    <FormField
                                        control={form.control}
                                        name="customer.name"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Customer Name *</FormLabel>
                                                <FormControl>
                                                    <Input
                                                        placeholder="Enter customer name"
                                                        {...field}
                                                        onChange={(e) => {
                                                            field.onChange(e);
                                                            handleCustomerUpdate('name', e.target.value);
                                                        }}
                                                    />
                                                </FormControl>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name="customer.phone"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Phone Number</FormLabel>
                                                <FormControl>
                                                    <Input
                                                        placeholder="Enter phone number"
                                                        {...field}
                                                        onChange={(e) => {
                                                            field.onChange(e);
                                                            handleCustomerUpdate('phone', e.target.value);
                                                        }}
                                                    />
                                                </FormControl>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />
                                </div>
                                <FormField
                                    control={form.control}
                                    name="customer.email"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Email Address</FormLabel>
                                            <FormControl>
                                                <Input
                                                    type="email"
                                                    placeholder="Enter email address"
                                                    {...field}
                                                    onChange={(e) => {
                                                        field.onChange(e);
                                                        handleCustomerUpdate('email', e.target.value);
                                                    }}
                                                />
                                            </FormControl>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />
                            </div>

                            <Separator />

                            {/* Payment Plan Configuration */}
                            <div className="space-y-4">
                                <div className="flex items-center gap-2">
                                    <Calculator className="h-4 w-4" />
                                    <h4 className="font-medium">Payment Plan Configuration</h4>
                                </div>

                                <div className="grid grid-cols-2 gap-4">
                                    <FormField
                                        control={form.control}
                                        name="downPayment"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Down Payment *</FormLabel>
                                                <FormControl>
                                                    <Input
                                                        type="number"
                                                        step="0.01"
                                                        min={totalAmount * 0.1}
                                                        max={totalAmount * 0.9}
                                                        {...field}
                                                        onChange={(e) => field.onChange(parseFloat(e.target.value) || 0)}
                                                    />
                                                </FormControl>
                                                <FormDescription>
                                                    Minimum 10% ({formatPrice(totalAmount * 0.1)})
                                                </FormDescription>
                                                <div className="flex gap-2 mt-2">
                                                    <Button
                                                        type="button"
                                                        variant="outline"
                                                        size="sm"
                                                        onClick={() => setDownPaymentPercentage(0.25)}
                                                    >
                                                        25%
                                                    </Button>
                                                    <Button
                                                        type="button"
                                                        variant="outline"
                                                        size="sm"
                                                        onClick={() => setDownPaymentPercentage(0.5)}
                                                    >
                                                        50%
                                                    </Button>
                                                    <Button
                                                        type="button"
                                                        variant="outline"
                                                        size="sm"
                                                        onClick={() => setDownPaymentPercentage(0.75)}
                                                    >
                                                        75%
                                                    </Button>
                                                </div>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />

                                    <FormField
                                        control={form.control}
                                        name="planDurationMonths"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Payment Duration *</FormLabel>
                                                <Select
                                                    value={field.value?.toString()}
                                                    onValueChange={(value) => field.onChange(parseInt(value))}
                                                >
                                                    <FormControl>
                                                        <SelectTrigger>
                                                            <SelectValue placeholder="Select duration" />
                                                        </SelectTrigger>
                                                    </FormControl>
                                                    <SelectContent>
                                                        <SelectItem value="3">3 months</SelectItem>
                                                        <SelectItem value="6">6 months</SelectItem>
                                                        <SelectItem value="12">12 months</SelectItem>
                                                        <SelectItem value="24">24 months</SelectItem>
                                                        <SelectItem value="36">36 months</SelectItem>
                                                    </SelectContent>
                                                </Select>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />
                                </div>

                                <FormField
                                    control={form.control}
                                    name="interestRate"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Interest Rate (% per year)</FormLabel>
                                            <FormControl>
                                                <Input
                                                    type="number"
                                                    step="0.1"
                                                    min="0"
                                                    max="30"
                                                    placeholder="0"
                                                    {...field}
                                                    onChange={(e) => field.onChange(parseFloat(e.target.value) || 0)}
                                                />
                                            </FormControl>
                                            <FormDescription>
                                                Enter 0 for interest-free payment plan
                                            </FormDescription>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />
                            </div>
                        </div>
                    </Form>
                </CardContent>
            </Card>

            {calculatedPlan && (
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <DollarSign className="h-5 w-5" />
                            Payment Summary
                        </CardTitle>
                    </CardHeader>
                    <CardContent>
                        <div className="grid grid-cols-2 gap-4">
                            <div>
                                <Label className="text-muted-foreground">Total Amount</Label>
                                <p className="text-lg font-semibold">{formatPrice(calculatedPlan.totalAmount)}</p>
                            </div>
                            <div>
                                <Label className="text-muted-foreground">Down Payment</Label>
                                <p className="text-lg font-semibold text-green-600">{formatPrice(calculatedPlan.downPayment)}</p>
                            </div>
                            <div>
                                <Label className="text-muted-foreground">Remaining Amount</Label>
                                <p className="text-lg font-semibold text-orange-600">{formatPrice(calculatedPlan.remainingAmount)}</p>
                            </div>
                            <div>
                                <Label className="text-muted-foreground">Monthly Payment</Label>
                                <p className="text-lg font-semibold text-blue-600">{formatPrice(calculatedPlan.monthlyPayment)}</p>
                            </div>
                        </div>

                        {calculatedPlan.interestRate > 0 && (
                            <div className="mt-4 p-3 bg-blue-50 rounded-md">
                                <p className="text-sm text-blue-700">
                                    Interest Rate: {calculatedPlan.interestRate}% per year
                                </p>
                            </div>
                        )}
                    </CardContent>
                </Card>
            )}
        </div>
    );
} 