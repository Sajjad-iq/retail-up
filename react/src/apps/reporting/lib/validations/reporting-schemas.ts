import { z } from 'zod';
import type { ReportType, ExportFormat } from '../../types/reporting';

/**
 * Helper function to validate date ranges
 */
const isValidDateRange = (dateRange: { startDate: Date; endDate: Date }): boolean => {
    return dateRange.startDate <= dateRange.endDate;
};

/**
 * Helper function to validate export formats
 */
const isValidExportFormat = (format: string): boolean => {
    const validFormats = ['pdf', 'excel', 'csv', 'json'];
    return validFormats.includes(format);
};

/**
 * Date range validation schema
 */
export const dateRangeSchema = z.object({
    startDate: z.date({
        required_error: 'Start date is required',
        invalid_type_error: 'Start date must be a valid date',
    }),
    endDate: z.date({
        required_error: 'End date is required',
        invalid_type_error: 'End date must be a valid date',
    }),
}).refine((data) => isValidDateRange(data), {
    message: 'End date must be after start date',
    path: ['endDate'],
});

/**
 * Report filter validation schema
 */
export const reportFilterSchema = z.object({
    dateRange: dateRangeSchema,
    storeId: z.string().optional(),
    employeeId: z.string().optional(),
    categoryId: z.string().optional(),
});

/**
 * Financial report generation schema
 */
export const financialReportSchema = z.object({
    title: z.string()
        .min(1, 'Report title is required')
        .min(3, 'Report title must be at least 3 characters')
        .max(100, 'Report title must not exceed 100 characters')
        .transform(val => val.trim()),
    period: dateRangeSchema,
    includeExpenseBreakdown: z.boolean().optional().default(true),
    includeRevenueBreakdown: z.boolean().optional().default(true),
    includeTrends: z.boolean().optional().default(true),
});

/**
 * Sales analytics generation schema
 */
export const salesAnalyticsSchema = z.object({
    period: dateRangeSchema,
    includeHourlyData: z.boolean().optional().default(true),
    includeDailyData: z.boolean().optional().default(true),
    includeCustomerAnalytics: z.boolean().optional().default(true),
    includeProductPerformance: z.boolean().optional().default(true),
    topProductsLimit: z.number()
        .min(1, 'Must include at least 1 top product')
        .max(50, 'Cannot include more than 50 top products')
        .optional()
        .default(10),
});

/**
 * Product performance report schema
 */
export const productPerformanceSchema = z.object({
    period: dateRangeSchema,
    categoryId: z.string().optional(),
    includeABCAnalysis: z.boolean().optional().default(true),
    includeTrendAnalysis: z.boolean().optional().default(true),
    minQuantitySold: z.number()
        .min(0, 'Minimum quantity sold cannot be negative')
        .optional()
        .default(0),
});

/**
 * Employee performance report schema
 */
export const employeePerformanceSchema = z.object({
    period: dateRangeSchema,
    employeeId: z.string().optional(),
    departmentId: z.string().optional(),
    includeCustomerSatisfaction: z.boolean().optional().default(false),
    includeUpsellMetrics: z.boolean().optional().default(true),
    minimumTransactions: z.number()
        .min(1, 'Minimum transactions must be at least 1')
        .optional()
        .default(10),
});

/**
 * Export request validation schema
 */
export const exportRequestSchema = z.object({
    reportType: z.enum([
        'financial',
        'sales-analytics',
        'product-performance',
        'inventory-analysis',
        'customer-analytics',
        'employee-performance',
        'profit-loss',
        'cash-flow'
    ] as const, {
        errorMap: () => ({ message: 'Invalid report type' }),
    }),
    format: z.enum(['pdf', 'excel', 'csv', 'json'] as const, {
        errorMap: () => ({ message: 'Invalid export format' }),
    }),
    filters: reportFilterSchema,
    includeCharts: z.boolean().optional().default(true),
    includeRawData: z.boolean().optional().default(false),
    filename: z.string()
        .min(1, 'Filename is required')
        .max(255, 'Filename too long')
        .regex(/^[a-zA-Z0-9_\-\s\.]+$/, 'Filename contains invalid characters')
        .optional(),
});

/**
 * Search and filter schema for reports
 */
export const reportSearchSchema = z.object({
    query: z.string().max(100, 'Search query too long').optional(),
    reportType: z.enum([
        'financial',
        'sales-analytics',
        'product-performance',
        'inventory-analysis',
        'customer-analytics',
        'employee-performance',
        'profit-loss',
        'cash-flow'
    ] as const).optional(),
    dateFrom: z.date().optional(),
    dateTo: z.date().optional(),
    storeId: z.string().optional(),
    sortBy: z.enum(['date', 'title', 'type', 'revenue', 'profit']).optional(),
    sortOrder: z.enum(['asc', 'desc']).optional().default('desc'),
});

/**
 * Dashboard metrics schema
 */
export const dashboardMetricsSchema = z.object({
    period: dateRangeSchema,
    includePreviousPeriod: z.boolean().optional().default(true),
    metrics: z.array(z.enum([
        'revenue',
        'profit',
        'transactions',
        'customers',
        'products',
        'inventory-value'
    ])).min(1, 'At least one metric must be selected'),
});

/**
 * Custom report builder schema
 */
export const customReportSchema = z.object({
    title: z.string()
        .min(1, 'Report title is required')
        .min(3, 'Report title must be at least 3 characters')
        .max(100, 'Report title must not exceed 100 characters'),
    description: z.string()
        .max(500, 'Description must not exceed 500 characters')
        .optional(),
    period: dateRangeSchema,
    dataPoints: z.array(z.string()).min(1, 'At least one data point must be selected'),
    groupBy: z.enum(['day', 'week', 'month', 'quarter', 'year']).optional(),
    filters: reportFilterSchema.optional(),
    chartType: z.enum(['line', 'bar', 'pie', 'area', 'table']).optional().default('line'),
});

// Type exports
export type DateRangeInput = z.infer<typeof dateRangeSchema>;
export type ReportFilterInput = z.infer<typeof reportFilterSchema>;
export type FinancialReportInput = z.infer<typeof financialReportSchema>;
export type SalesAnalyticsInput = z.infer<typeof salesAnalyticsSchema>;
export type ProductPerformanceInput = z.infer<typeof productPerformanceSchema>;
export type EmployeePerformanceInput = z.infer<typeof employeePerformanceSchema>;
export type ExportRequestInput = z.infer<typeof exportRequestSchema>;
export type ReportSearchInput = z.infer<typeof reportSearchSchema>;
export type DashboardMetricsInput = z.infer<typeof dashboardMetricsSchema>;
export type CustomReportInput = z.infer<typeof customReportSchema>;

// Legacy function exports for backward compatibility
export const validateFinancialReport = (reportData: any) => {
    const result = financialReportSchema.safeParse(reportData);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
};

export const validateExportRequest = (exportData: any) => {
    const result = exportRequestSchema.safeParse(exportData);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
};

export const validateReportFilters = (filterData: any) => {
    const result = reportFilterSchema.safeParse(filterData);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
}; 