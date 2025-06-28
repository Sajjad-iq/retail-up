/**
 * Customer information
 */
export interface Customer {
    /** Unique customer identifier */
    id: string;
    /** Customer name */
    name: string;
    /** Optional email address */
    email?: string;
    /** Optional phone number */
    phone?: string;
    /** Customer address */
    address?: string;
    /** Loyalty points */
    loyaltyPoints?: number;
    /** Customer type */
    type?: 'regular' | 'vip' | 'wholesale';
}

/**
 * Payment method types
 */
export type PaymentMethod = 'cash' | 'card' | 'mobile' | 'check' | 'giftcard' | 'loyalty';

/**
 * Payment plan details for installment payments
 */
export interface PaymentPlan {
    /** Unique payment plan identifier */
    id: string;
    /** Original transaction ID */
    transactionId: string;
    /** Customer information */
    customer: Customer;
    /** Total amount to be paid */
    totalAmount: number;
    /** Amount paid upfront */
    downPayment: number;
    /** Remaining amount to be paid */
    remainingAmount: number;
    /** Monthly payment amount */
    monthlyPayment: number;
    /** Number of months for payment plan */
    planDurationMonths: number;
    /** Interest rate percentage */
    interestRate: number;
    /** Payment plan status */
    status: 'active' | 'completed' | 'overdue' | 'cancelled';
    /** Created timestamp */
    createdAt: Date;
    /** Start date of payment plan */
    startDate: Date;
    /** List of scheduled payments */
    scheduledPayments: ScheduledPayment[];
}

/**
 * Scheduled payment for installment plans
 */
export interface ScheduledPayment {
    /** Unique payment identifier */
    id: string;
    /** Payment plan ID */
    planId: string;
    /** Due date */
    dueDate: Date;
    /** Payment amount */
    amount: number;
    /** Payment status */
    status: 'pending' | 'paid' | 'overdue' | 'partial';
    /** Amount paid */
    paidAmount?: number;
    /** Payment date */
    paidDate?: Date;
    /** Payment method used */
    paymentMethod?: PaymentMethod;
    /** Late fees applied */
    lateFees?: number;
}

/**
 * Payment plan analytics data
 */
export interface PaymentPlanAnalytics {
    /** Total active plans */
    activePlans: number;
    /** Total completed plans */
    completedPlans: number;
    /** Total overdue plans */
    overduePlans: number;
    /** Total revenue from payment plans */
    totalRevenue: number;
    /** Average plan duration */
    averagePlanDuration: number;
    /** Monthly collection rate */
    monthlyCollectionRate: number;
}

/**
 * Payment plan state
 */
export interface PaymentPlanState {
    /** Active payment plans */
    paymentPlans: PaymentPlan[];
    /** Loading state */
    isLoading: boolean;
    /** Error state */
    error: string | null;
    /** Selected payment plan for details */
    selectedPlan: PaymentPlan | null;
}

/**
 * Payment plan form data
 */
export interface PaymentPlanFormData {
    /** Customer information */
    customer: Customer;
    /** Down payment amount */
    downPayment: number;
    /** Total amount to be financed */
    totalAmount: number;
    /** Plan duration in months */
    planDurationMonths: number;
    /** Interest rate percentage */
    interestRate: number;
}

/**
 * Payment collection data
 */
export interface PaymentCollectionData {
    /** Payment plan ID */
    planId: string;
    /** Scheduled payment ID */
    scheduledPaymentId: string;
    /** Amount being paid */
    amount: number;
    /** Payment method */
    paymentMethod: PaymentMethod;
    /** Payment date */
    paymentDate: Date;
    /** Late fees if applicable */
    lateFees?: number;
} 