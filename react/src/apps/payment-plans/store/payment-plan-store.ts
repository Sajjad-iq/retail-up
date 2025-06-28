import { create } from 'zustand';
import { devtools } from 'zustand/middleware';
import type {
    PaymentPlan,
    ScheduledPayment,
    PaymentPlanState,
    PaymentCollectionData,
    Customer
} from '../types/payment-plans';
import {
    calculateMonthlyPayment,
    generateScheduledPayments,
    calculatePaymentPlanAnalytics,
    getOverduePayments,
    getUpcomingPayments
} from '../lib/utils/payment-plan-utils';

interface PaymentPlanStore extends PaymentPlanState {
    // Payment Plan Actions
    createPaymentPlan: (planData: {
        customer: Customer;
        totalAmount: number;
        downPayment: number;
        planDurationMonths: number;
        interestRate: number;
        transactionId?: string;
    }) => PaymentPlan;
    updatePaymentPlan: (planId: string, updates: Partial<PaymentPlan>) => void;
    deletePaymentPlan: (planId: string) => void;
    getPaymentPlan: (planId: string) => PaymentPlan | undefined;
    setSelectedPlan: (plan: PaymentPlan | null) => void;

    // Payment Collection Actions
    recordPayment: (collectionData: PaymentCollectionData) => boolean;
    markPaymentOverdue: (paymentId: string) => void;
    addLateFees: (paymentId: string, lateFees: number) => void;

    // Query Actions
    getActivePaymentPlans: () => PaymentPlan[];
    getCompletedPaymentPlans: () => PaymentPlan[];
    getOverduePaymentPlans: () => PaymentPlan[];
    getPaymentPlansByCustomer: (customerId: string) => PaymentPlan[];
    getAllOverduePayments: () => ScheduledPayment[];
    getAllUpcomingPayments: () => ScheduledPayment[];

    // Analytics
    getAnalytics: () => ReturnType<typeof calculatePaymentPlanAnalytics>;

    // Utility Actions
    setLoading: (loading: boolean) => void;
    setError: (error: string | null) => void;
    clearError: () => void;

    // Data Management
    loadPaymentPlans: (plans: PaymentPlan[]) => void;
    clearAllData: () => void;
}

/**
 * Payment Plans Store
 * 
 * Manages payment plan state and operations using Zustand.
 * Provides actions for creating, updating, and managing payment plans and scheduled payments.
 */
export const usePaymentPlanStore = create<PaymentPlanStore>()(
    devtools(
        (set, get) => ({
            // Initial State
            paymentPlans: [],
            isLoading: false,
            error: null,
            selectedPlan: null,

            // Payment Plan Actions
            /**
             * Create a new payment plan
             */
            createPaymentPlan: (planData) => {
                const { customer, totalAmount, downPayment, planDurationMonths, interestRate, transactionId } = planData;

                const remainingAmount = totalAmount - downPayment;
                const monthlyPayment = calculateMonthlyPayment(remainingAmount, interestRate, planDurationMonths);
                const now = new Date();
                const planId = `plan-${Date.now()}`;

                const scheduledPayments = generateScheduledPayments(
                    planId,
                    monthlyPayment,
                    planDurationMonths,
                    now
                );

                const newPlan: PaymentPlan = {
                    id: planId,
                    transactionId: transactionId || '',
                    customer,
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

                set((state) => ({
                    paymentPlans: [newPlan, ...state.paymentPlans],
                }));

                return newPlan;
            },

            /**
             * Update an existing payment plan
             */
            updatePaymentPlan: (planId, updates) => {
                set((state) => ({
                    paymentPlans: state.paymentPlans.map(plan =>
                        plan.id === planId ? { ...plan, ...updates } : plan
                    ),
                    selectedPlan: state.selectedPlan?.id === planId
                        ? { ...state.selectedPlan, ...updates }
                        : state.selectedPlan,
                }));
            },

            /**
             * Delete a payment plan
             */
            deletePaymentPlan: (planId) => {
                set((state) => ({
                    paymentPlans: state.paymentPlans.filter(plan => plan.id !== planId),
                    selectedPlan: state.selectedPlan?.id === planId ? null : state.selectedPlan,
                }));
            },

            /**
             * Get a specific payment plan by ID
             */
            getPaymentPlan: (planId) => {
                const { paymentPlans } = get();
                return paymentPlans.find(plan => plan.id === planId);
            },

            /**
             * Set the selected payment plan for details view
             */
            setSelectedPlan: (plan) => {
                set({ selectedPlan: plan });
            },

            // Payment Collection Actions
            /**
             * Record a payment for a scheduled payment
             */
            recordPayment: (collectionData) => {
                const { planId, scheduledPaymentId, amount, paymentMethod, paymentDate, lateFees } = collectionData;

                set((state) => {
                    const planIndex = state.paymentPlans.findIndex(plan => plan.id === planId);
                    if (planIndex === -1) return state;

                    const updatedPlans = [...state.paymentPlans];
                    const plan = { ...updatedPlans[planIndex] };
                    const paymentIndex = plan.scheduledPayments.findIndex(p => p.id === scheduledPaymentId);

                    if (paymentIndex === -1) return state;

                    const payment = { ...plan.scheduledPayments[paymentIndex] };

                    // Update payment status and details
                    if (amount >= payment.amount) {
                        payment.status = 'paid';
                        payment.paidAmount = amount;
                    } else {
                        payment.status = 'partial';
                        payment.paidAmount = amount;
                    }

                    payment.paidDate = paymentDate;
                    payment.paymentMethod = paymentMethod;
                    if (lateFees) payment.lateFees = lateFees;

                    plan.scheduledPayments[paymentIndex] = payment;

                    // Check if all payments are complete
                    const allPaid = plan.scheduledPayments.every(p => p.status === 'paid');
                    if (allPaid) {
                        plan.status = 'completed';
                    }

                    updatedPlans[planIndex] = plan;

                    return {
                        ...state,
                        paymentPlans: updatedPlans,
                        selectedPlan: state.selectedPlan?.id === planId ? plan : state.selectedPlan,
                    };
                });

                return true;
            },

            /**
             * Mark a payment as overdue
             */
            markPaymentOverdue: (paymentId) => {
                set((state) => ({
                    paymentPlans: state.paymentPlans.map(plan => ({
                        ...plan,
                        scheduledPayments: plan.scheduledPayments.map(payment =>
                            payment.id === paymentId
                                ? { ...payment, status: 'overdue' as const }
                                : payment
                        ),
                    })),
                }));
            },

            /**
             * Add late fees to a payment
             */
            addLateFees: (paymentId, lateFees) => {
                set((state) => ({
                    paymentPlans: state.paymentPlans.map(plan => ({
                        ...plan,
                        scheduledPayments: plan.scheduledPayments.map(payment =>
                            payment.id === paymentId
                                ? { ...payment, lateFees: (payment.lateFees || 0) + lateFees }
                                : payment
                        ),
                    })),
                }));
            },

            // Query Actions
            /**
             * Get all active payment plans
             */
            getActivePaymentPlans: () => {
                const { paymentPlans } = get();
                return paymentPlans.filter(plan => plan.status === 'active');
            },

            /**
             * Get all completed payment plans
             */
            getCompletedPaymentPlans: () => {
                const { paymentPlans } = get();
                return paymentPlans.filter(plan => plan.status === 'completed');
            },

            /**
             * Get all overdue payment plans
             */
            getOverduePaymentPlans: () => {
                const { paymentPlans } = get();
                return paymentPlans.filter(plan => plan.status === 'overdue');
            },

            /**
             * Get payment plans for a specific customer
             */
            getPaymentPlansByCustomer: (customerId) => {
                const { paymentPlans } = get();
                return paymentPlans.filter(plan => plan.customer.id === customerId);
            },

            /**
             * Get all overdue payments across all plans
             */
            getAllOverduePayments: () => {
                const { paymentPlans } = get();
                return paymentPlans.flatMap(plan => getOverduePayments(plan));
            },

            /**
             * Get all upcoming payments across all plans
             */
            getAllUpcomingPayments: () => {
                const { paymentPlans } = get();
                return paymentPlans.flatMap(plan => getUpcomingPayments(plan));
            },

            // Analytics
            /**
             * Get payment plan analytics
             */
            getAnalytics: () => {
                const { paymentPlans } = get();
                return calculatePaymentPlanAnalytics(paymentPlans);
            },

            // Utility Actions
            /**
             * Set loading state
             */
            setLoading: (loading) => {
                set({ isLoading: loading });
            },

            /**
             * Set error state
             */
            setError: (error) => {
                set({ error });
            },

            /**
             * Clear error state
             */
            clearError: () => {
                set({ error: null });
            },

            // Data Management
            /**
             * Load payment plans (for initialization)
             */
            loadPaymentPlans: (plans) => {
                set({ paymentPlans: plans });
            },

            /**
             * Clear all data
             */
            clearAllData: () => {
                set({
                    paymentPlans: [],
                    selectedPlan: null,
                    error: null,
                    isLoading: false,
                });
            },
        }),
        {
            name: 'payment-plan-store',
        }
    )
); 