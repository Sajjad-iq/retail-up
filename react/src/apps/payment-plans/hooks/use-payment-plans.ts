import { useCallback, useMemo } from 'react';
import { usePaymentPlanStore } from '../store/payment-plan-store';
import type { PaymentPlan, Customer, PaymentCollectionData } from '../types/payment-plans';
import { formatPrice, formatDate, calculateRemainingBalance, calculateCompletionPercentage } from '../lib/utils/payment-plan-utils';

/**
 * Custom hook for payment plan operations
 * 
 * Provides a clean interface for managing payment plans with computed values and memoized callbacks.
 */
export function usePaymentPlans() {
    const {
        paymentPlans,
        isLoading,
        error,
        selectedPlan,
        createPaymentPlan,
        updatePaymentPlan,
        deletePaymentPlan,
        getPaymentPlan,
        setSelectedPlan,
        recordPayment,
        markPaymentOverdue,
        addLateFees,
        getActivePaymentPlans,
        getCompletedPaymentPlans,
        getOverduePaymentPlans,
        getPaymentPlansByCustomer,
        getAllOverduePayments,
        getAllUpcomingPayments,
        getAnalytics,
        setLoading,
        setError,
        clearError,
        loadPaymentPlans,
        clearAllData,
    } = usePaymentPlanStore();

    // Memoized computed values
    const computedValues = useMemo(() => {
        const analytics = getAnalytics();
        const activePlans = getActivePaymentPlans();
        const overduePayments = getAllOverduePayments();
        const upcomingPayments = getAllUpcomingPayments();

        return {
            // Counts
            totalPlans: paymentPlans.length,
            activePlansCount: analytics.activePlans,
            completedPlansCount: analytics.completedPlans,
            overduePlansCount: analytics.overduePlans,

            // Collections
            activePlans,
            overduePayments,
            upcomingPayments,

            // Analytics
            analytics,

            // Financial
            totalRevenue: formatPrice(analytics.totalRevenue),
            averagePlanDuration: Math.round(analytics.averagePlanDuration),
            collectionRate: Math.round(analytics.monthlyCollectionRate),
        };
    }, [paymentPlans, getAnalytics, getActivePaymentPlans, getAllOverduePayments, getAllUpcomingPayments]);

    // Memoized callbacks to prevent unnecessary re-renders
    const callbacks = useMemo(() => ({
        /**
         * Create a new payment plan with validation
         */
        createPlan: (planData: {
            customer: Customer;
            totalAmount: number;
            downPayment: number;
            planDurationMonths: number;
            interestRate: number;
            transactionId?: string;
        }) => {
            try {
                const plan = createPaymentPlan(planData);
                return { success: true, plan };
            } catch (error) {
                setError(error instanceof Error ? error.message : 'Failed to create payment plan');
                return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
            }
        },

        /**
         * Update a payment plan
         */
        updatePlan: (planId: string, updates: Partial<PaymentPlan>) => {
            try {
                updatePaymentPlan(planId, updates);
                return { success: true };
            } catch (error) {
                setError(error instanceof Error ? error.message : 'Failed to update payment plan');
                return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
            }
        },

        /**
         * Delete a payment plan with confirmation
         */
        deletePlan: (planId: string) => {
            try {
                deletePaymentPlan(planId);
                return { success: true };
            } catch (error) {
                setError(error instanceof Error ? error.message : 'Failed to delete payment plan');
                return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
            }
        },

        /**
         * Process a payment collection
         */
        processPayment: (collectionData: PaymentCollectionData) => {
            try {
                const success = recordPayment(collectionData);
                if (success) {
                    return { success: true };
                } else {
                    throw new Error('Payment processing failed');
                }
            } catch (error) {
                setError(error instanceof Error ? error.message : 'Failed to process payment');
                return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
            }
        },

        /**
         * Search payment plans by customer name
         */
        searchByCustomer: (customerName: string) => {
            return paymentPlans.filter(plan =>
                plan.customer.name.toLowerCase().includes(customerName.toLowerCase())
            );
        },

        /**
         * Filter payment plans by status
         */
        filterByStatus: (status: PaymentPlan['status']) => {
            switch (status) {
                case 'active':
                    return getActivePaymentPlans();
                case 'completed':
                    return getCompletedPaymentPlans();
                case 'overdue':
                    return getOverduePaymentPlans();
                default:
                    return paymentPlans.filter(plan => plan.status === status);
            }
        },

        /**
         * Get customer payment plans
         */
        getCustomerPlans: (customerId: string) => {
            return getPaymentPlansByCustomer(customerId);
        },

        /**
         * Get payment plan with computed values
         */
        getPlanWithDetails: (planId: string) => {
            const plan = getPaymentPlan(planId);
            if (!plan) return null;

            return {
                ...plan,
                remainingBalance: calculateRemainingBalance(plan),
                completionPercentage: calculateCompletionPercentage(plan),
                formattedTotalAmount: formatPrice(plan.totalAmount),
                formattedDownPayment: formatPrice(plan.downPayment),
                formattedMonthlyPayment: formatPrice(plan.monthlyPayment),
                formattedCreatedAt: formatDate(plan.createdAt),
                formattedStartDate: formatDate(plan.startDate),
            };
        },
    }), [
        paymentPlans,
        createPaymentPlan,
        updatePaymentPlan,
        deletePaymentPlan,
        getPaymentPlan,
        recordPayment,
        getActivePaymentPlans,
        getCompletedPaymentPlans,
        getOverduePaymentPlans,
        getPaymentPlansByCustomer,
        setError,
    ]);

    // Loading and error management
    const handleAsyncOperation = useCallback(async (operation: () => Promise<void>) => {
        setLoading(true);
        clearError();
        try {
            await operation();
        } catch (error) {
            setError(error instanceof Error ? error.message : 'An error occurred');
        } finally {
            setLoading(false);
        }
    }, [setLoading, setError, clearError]);

    return {
        // State
        paymentPlans,
        isLoading,
        error,
        selectedPlan,

        // Computed values
        ...computedValues,

        // Actions
        ...callbacks,
        setSelectedPlan,
        markPaymentOverdue,
        addLateFees,
        loadPaymentPlans,
        clearAllData,
        clearError,
        handleAsyncOperation,

        // Raw store actions (for advanced use)
        store: {
            createPaymentPlan,
            updatePaymentPlan,
            deletePaymentPlan,
            getPaymentPlan,
            recordPayment,
            getActivePaymentPlans,
            getCompletedPaymentPlans,
            getOverduePaymentPlans,
            getPaymentPlansByCustomer,
            getAllOverduePayments,
            getAllUpcomingPayments,
            getAnalytics,
        },
    };
} 