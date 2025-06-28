import type { PaymentPlan, ScheduledPayment, PaymentPlanAnalytics } from '../../types/payment-plans';

/**
 * Format currency with proper locale
 */
export function formatPrice(amount: number): string {
    return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD',
    }).format(amount);
}

/**
 * Format date with proper locale
 */
export function formatDate(date: Date): string {
    return new Intl.DateTimeFormat('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
    }).format(date);
}

/**
 * Format date and time with proper locale
 */
export function formatDateTime(date: Date): string {
    return new Intl.DateTimeFormat('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
    }).format(date);
}

/**
 * Calculate monthly payment with interest
 */
export function calculateMonthlyPayment(
    remainingAmount: number,
    interestRate: number,
    months: number
): number {
    if (interestRate === 0) {
        return remainingAmount / months;
    }

    const monthlyInterestRate = interestRate / 100 / 12;
    return remainingAmount *
        (monthlyInterestRate * Math.pow(1 + monthlyInterestRate, months)) /
        (Math.pow(1 + monthlyInterestRate, months) - 1);
}

/**
 * Generate scheduled payments for a payment plan
 */
export function generateScheduledPayments(
    planId: string,
    monthlyPayment: number,
    months: number,
    startDate: Date = new Date()
): ScheduledPayment[] {
    return Array.from({ length: months }, (_, index) => {
        const dueDate = new Date(startDate);
        dueDate.setMonth(dueDate.getMonth() + index + 1);

        return {
            id: `payment-${planId}-${index + 1}`,
            planId,
            dueDate,
            amount: monthlyPayment,
            status: 'pending',
        };
    });
}

/**
 * Check if a payment is overdue
 */
export function isPaymentOverdue(dueDate: Date): boolean {
    const today = new Date();
    today.setHours(23, 59, 59, 999); // End of today
    return dueDate < today;
}

/**
 * Calculate days until due
 */
export function getDaysUntilDue(dueDate: Date): number {
    const today = new Date();
    const timeDiff = dueDate.getTime() - today.getTime();
    return Math.ceil(timeDiff / (1000 * 3600 * 24));
}

/**
 * Calculate total remaining balance for a payment plan
 */
export function calculateRemainingBalance(plan: PaymentPlan): number {
    const paidAmount = plan.scheduledPayments
        .filter(payment => payment.status === 'paid')
        .reduce((sum, payment) => sum + (payment.paidAmount || payment.amount), 0);

    return plan.remainingAmount - paidAmount;
}

/**
 * Calculate completion percentage for a payment plan
 */
export function calculateCompletionPercentage(plan: PaymentPlan): number {
    const totalScheduled = plan.scheduledPayments.reduce((sum, payment) => sum + payment.amount, 0);
    const totalPaid = plan.scheduledPayments
        .filter(payment => payment.status === 'paid')
        .reduce((sum, payment) => sum + (payment.paidAmount || payment.amount), 0);

    return totalScheduled > 0 ? (totalPaid / totalScheduled) * 100 : 0;
}

/**
 * Get overdue payments for a payment plan
 */
export function getOverduePayments(plan: PaymentPlan): ScheduledPayment[] {
    return plan.scheduledPayments.filter(payment =>
        payment.status === 'pending' && isPaymentOverdue(payment.dueDate)
    );
}

/**
 * Get upcoming payments (within next 7 days)
 */
export function getUpcomingPayments(plan: PaymentPlan): ScheduledPayment[] {
    const sevenDaysFromNow = new Date();
    sevenDaysFromNow.setDate(sevenDaysFromNow.getDate() + 7);

    return plan.scheduledPayments.filter(payment =>
        payment.status === 'pending' &&
        payment.dueDate <= sevenDaysFromNow &&
        !isPaymentOverdue(payment.dueDate)
    );
}

/**
 * Calculate payment plan analytics
 */
export function calculatePaymentPlanAnalytics(plans: PaymentPlan[]): PaymentPlanAnalytics {
    const activePlans = plans.filter(plan => plan.status === 'active').length;
    const completedPlans = plans.filter(plan => plan.status === 'completed').length;
    const overduePlans = plans.filter(plan => plan.status === 'overdue').length;

    const totalRevenue = plans.reduce((sum, plan) => {
        const paidAmount = plan.scheduledPayments
            .filter(payment => payment.status === 'paid')
            .reduce((total, payment) => total + (payment.paidAmount || payment.amount), 0);
        return sum + plan.downPayment + paidAmount;
    }, 0);

    const averagePlanDuration = plans.length > 0
        ? plans.reduce((sum, plan) => sum + plan.planDurationMonths, 0) / plans.length
        : 0;

    // Calculate monthly collection rate (successful payments vs scheduled)
    const allScheduledPayments = plans.flatMap(plan => plan.scheduledPayments);
    const paidPayments = allScheduledPayments.filter(payment => payment.status === 'paid');
    const monthlyCollectionRate = allScheduledPayments.length > 0
        ? (paidPayments.length / allScheduledPayments.length) * 100
        : 0;

    return {
        activePlans,
        completedPlans,
        overduePlans,
        totalRevenue,
        averagePlanDuration,
        monthlyCollectionRate,
    };
}

/**
 * Get payment plan status color for UI
 */
export function getPaymentPlanStatusColor(status: PaymentPlan['status']): string {
    switch (status) {
        case 'active':
            return 'text-blue-600 bg-blue-50 border-blue-200';
        case 'completed':
            return 'text-green-600 bg-green-50 border-green-200';
        case 'overdue':
            return 'text-red-600 bg-red-50 border-red-200';
        case 'cancelled':
            return 'text-gray-600 bg-gray-50 border-gray-200';
        default:
            return 'text-gray-600 bg-gray-50 border-gray-200';
    }
}

/**
 * Get scheduled payment status color for UI
 */
export function getScheduledPaymentStatusColor(status: ScheduledPayment['status']): string {
    switch (status) {
        case 'pending':
            return 'text-yellow-600 bg-yellow-50 border-yellow-200';
        case 'paid':
            return 'text-green-600 bg-green-50 border-green-200';
        case 'overdue':
            return 'text-red-600 bg-red-50 border-red-200';
        case 'partial':
            return 'text-orange-600 bg-orange-50 border-orange-200';
        default:
            return 'text-gray-600 bg-gray-50 border-gray-200';
    }
}

/**
 * Validate minimum down payment percentage
 */
export function validateDownPayment(downPayment: number, totalAmount: number): boolean {
    return downPayment >= totalAmount * 0.1 && downPayment <= totalAmount * 0.9;
}

/**
 * Calculate late fees based on overdue amount and days
 */
export function calculateLateFees(overdueAmount: number, daysOverdue: number): number {
    // 5% late fee after 30 days, additional 1% for each 30-day period
    if (daysOverdue <= 30) return 0;

    const lateFeePercentage = 0.05 + Math.floor((daysOverdue - 30) / 30) * 0.01;
    return overdueAmount * lateFeePercentage;
} 