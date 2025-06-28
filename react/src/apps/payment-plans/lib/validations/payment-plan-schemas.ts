import { z } from 'zod';

/**
 * Customer validation schema
 */
export const customerSchema = z.object({
    id: z.string().min(1, 'Customer ID is required'),
    name: z.string().min(1, 'Customer name is required'),
    email: z.string().email('Invalid email format').optional(),
    phone: z.string().min(10, 'Phone number must be at least 10 digits').optional(),
    address: z.string().optional(),
    loyaltyPoints: z.number().min(0, 'Loyalty points cannot be negative').optional(),
    type: z.enum(['regular', 'vip', 'wholesale']).optional(),
});

/**
 * Payment method validation schema
 */
export const paymentMethodSchema = z.enum(['cash', 'card', 'mobile', 'check', 'giftcard', 'loyalty'], {
    errorMap: () => ({ message: 'Invalid payment method' }),
});

/**
 * Payment plan validation schema
 */
export const paymentPlanSchema = z.object({
    planDurationMonths: z.number().int().min(1, 'Plan duration must be at least 1 month').max(60, 'Plan duration cannot exceed 60 months'),
    downPaymentPercentage: z.number().min(0.1, 'Down payment must be at least 10%').max(0.9, 'Down payment cannot exceed 90%'),
    interestRate: z.number().min(0, 'Interest rate cannot be negative').max(30, 'Interest rate cannot exceed 30%'),
});

/**
 * Payment plan form validation schema
 */
export const paymentPlanFormSchema = z.object({
    customer: customerSchema,
    downPayment: z.number().positive('Down payment must be positive'),
    totalAmount: z.number().positive('Total amount must be positive'),
    planDurationMonths: z.number().int().min(1, 'Duration must be at least 1 month').max(60, 'Duration cannot exceed 60 months'),
    interestRate: z.number().min(0, 'Interest rate cannot be negative').max(30, 'Interest rate cannot exceed 30%'),
}).refine((data) => data.downPayment <= data.totalAmount, {
    message: 'Down payment cannot exceed total amount',
    path: ['downPayment'],
}).refine((data) => data.downPayment >= data.totalAmount * 0.1, {
    message: 'Down payment must be at least 10% of total amount',
    path: ['downPayment'],
});

/**
 * Scheduled payment validation schema
 */
export const scheduledPaymentSchema = z.object({
    planId: z.string().min(1, 'Plan ID is required'),
    amount: z.number().positive('Payment amount must be positive'),
    paymentMethod: paymentMethodSchema,
    lateFees: z.number().min(0, 'Late fees cannot be negative').optional(),
});

/**
 * Payment collection validation schema
 */
export const paymentCollectionSchema = z.object({
    planId: z.string().min(1, 'Plan ID is required'),
    scheduledPaymentId: z.string().min(1, 'Scheduled payment ID is required'),
    amount: z.number().positive('Payment amount must be positive'),
    paymentMethod: paymentMethodSchema,
    paymentDate: z.date(),
    lateFees: z.number().min(0, 'Late fees cannot be negative').optional(),
});

/**
 * Payment plan update validation schema
 */
export const paymentPlanUpdateSchema = z.object({
    status: z.enum(['active', 'completed', 'overdue', 'cancelled']).optional(),
    monthlyPayment: z.number().positive('Monthly payment must be positive').optional(),
    interestRate: z.number().min(0, 'Interest rate cannot be negative').max(30, 'Interest rate cannot exceed 30%').optional(),
});

/**
 * Payment plan search validation schema
 */
export const paymentPlanSearchSchema = z.object({
    customerName: z.string().max(100, 'Customer name search too long').optional(),
    status: z.enum(['active', 'completed', 'overdue', 'cancelled']).optional(),
    dateFrom: z.date().optional(),
    dateTo: z.date().optional(),
}).refine((data) => {
    if (data.dateFrom && data.dateTo) {
        return data.dateFrom <= data.dateTo;
    }
    return true;
}, {
    message: 'From date must be before or equal to To date',
    path: ['dateTo'],
});

// Type exports
export type CustomerInput = z.infer<typeof customerSchema>;
export type PaymentPlanFormInput = z.infer<typeof paymentPlanFormSchema>;
export type ScheduledPaymentInput = z.infer<typeof scheduledPaymentSchema>;
export type PaymentCollectionInput = z.infer<typeof paymentCollectionSchema>;
export type PaymentPlanUpdateInput = z.infer<typeof paymentPlanUpdateSchema>;
export type PaymentPlanSearchInput = z.infer<typeof paymentPlanSearchSchema>; 