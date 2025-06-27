import { create } from 'zustand';
import { subscribeWithSelector } from 'zustand/middleware';
import type {
    ReportingState,
    FinancialReport,
    SalesAnalytics,
    EmployeePerformance,
    ReportFilter,
    ExportRequest,
    DateRange,
    ReportType
} from '../types/reporting';

/**
 * Initial state for the reporting store
 */
const initialState: ReportingState = {
    financialReports: [],
    salesAnalytics: [],
    employeePerformance: [],
    activeReport: null,
    loading: {
        generating: false,
        exporting: false,
        fetching: false,
    },
    errors: {
        generation: null,
        export: null,
        fetch: null,
    },
    filters: {
        dateRange: {
            startDate: new Date(new Date().getFullYear(), new Date().getMonth(), 1),
            endDate: new Date(),
        },
    },
};

/**
 * Reporting Store Interface
 * 
 * Defines the state and actions for the reporting system
 */
interface ReportingStore extends ReportingState {
    // Financial Reports
    generateFinancialReport: (filters: ReportFilter) => Promise<FinancialReport>;
    getFinancialReportById: (id: string) => FinancialReport | undefined;
    deleteFinancialReport: (id: string) => void;

    // Sales Analytics
    generateSalesAnalytics: (filters: ReportFilter) => Promise<SalesAnalytics>;
    getSalesAnalyticsById: (id: string) => SalesAnalytics | undefined;
    deleteSalesAnalytics: (id: string) => void;

    // Employee Performance
    generateEmployeePerformance: (filters: ReportFilter) => Promise<EmployeePerformance[]>;
    getEmployeePerformanceById: (id: string) => EmployeePerformance | undefined;

    // Export Functions
    exportReport: (request: ExportRequest) => Promise<{ success: boolean; url?: string; error?: string }>;

    // Report Management
    setActiveReport: (report: any) => void;
    clearActiveReport: () => void;
    setFilters: (filters: Partial<ReportFilter>) => void;
    clearErrors: () => void;

    // Utility Functions
    getReportsByDateRange: (dateRange: DateRange) => any[];
    getReportsByType: (type: ReportType) => any[];
    refreshReports: () => Promise<void>;

    // Reset
    reset: () => void;
}

/**
 * Reporting Store Implementation
 * 
 * Main store for managing reporting data and operations
 */
export const useReportingStore = create<ReportingStore>()(
    subscribeWithSelector((set, get) => ({
        ...initialState,

        /**
         * Generate a financial report
         */
        generateFinancialReport: async (filters: ReportFilter) => {
            set((state) => ({
                ...state,
                loading: { ...state.loading, generating: true },
            }));

            try {
                const report: FinancialReport = {
                    id: `financial-${Date.now()}`,
                    title: `Financial Report`,
                    period: filters.dateRange,
                    totalRevenue: 125000,
                    totalExpenses: 75000,
                    netProfit: 50000,
                    grossMargin: 65,
                    costOfGoodsSold: 45000,
                    operatingExpenses: [],
                    revenueByCategory: [],
                    generatedAt: new Date(),
                };

                set((state) => ({
                    ...state,
                    financialReports: [...state.financialReports, report],
                    activeReport: report,
                    loading: { ...state.loading, generating: false },
                }));

                return report;
            } catch (error) {
                set((state) => ({
                    ...state,
                    loading: { ...state.loading, generating: false },
                    errors: { ...state.errors, generation: 'Failed to generate report' },
                }));
                throw error;
            }
        },

        /**
         * Generate sales analytics
         */
        generateSalesAnalytics: async (filters: ReportFilter) => {
            set((state) => ({
                ...state,
                loading: { ...state.loading, generating: true },
            }));

            try {
                const analytics: SalesAnalytics = {
                    id: `sales-${Date.now()}`,
                    period: filters.dateRange,
                    totalSales: 100000,
                    totalTransactions: 1500,
                    averageTransactionValue: 66.67,
                    salesTrend: [],
                    topProducts: [],
                    salesByHour: [],
                    salesByDayOfWeek: [],
                    customerAnalytics: {
                        totalCustomers: 850,
                        newCustomers: 125,
                        returningCustomers: 725,
                        retentionRate: 85.3,
                        averageLifetimeValue: 450
                    },
                    paymentMethods: [],
                    generatedAt: new Date(),
                };

                set((state) => ({
                    ...state,
                    salesAnalytics: [...state.salesAnalytics, analytics],
                    activeReport: analytics,
                    loading: { ...state.loading, generating: false },
                }));

                return analytics;
            } catch (error) {
                set((state) => ({
                    ...state,
                    loading: { ...state.loading, generating: false },
                    errors: { ...state.errors, generation: 'Failed to generate analytics' },
                }));
                throw error;
            }
        },

        /**
         * Generate employee performance report
         */
        generateEmployeePerformance: async (filters: ReportFilter) => {
            set((state) => ({
                ...state,
                loading: { ...state.loading, generating: true },
            }));

            try {
                const employees: EmployeePerformance[] = [
                    {
                        employeeId: 'emp-1',
                        employeeName: 'John Smith',
                        role: 'Sales Associate',
                        totalSales: 45000,
                        transactionCount: 350,
                        averageTransactionValue: 128.57,
                        salesPerHour: 285,
                        upsellRate: 25,
                        trend: 'improving'
                    }
                ];

                set((state) => ({
                    ...state,
                    employeePerformance: employees,
                    loading: { ...state.loading, generating: false },
                }));

                return employees;
            } catch (error) {
                set((state) => ({
                    ...state,
                    loading: { ...state.loading, generating: false },
                    errors: { ...state.errors, generation: 'Failed to generate performance' },
                }));
                throw error;
            }
        },

        /**
         * Export a report
         */
        exportReport: async (request: ExportRequest) => {
            set((state) => ({
                ...state,
                loading: { ...state.loading, exporting: true },
            }));

            try {
                await new Promise(resolve => setTimeout(resolve, 2000));

                set((state) => ({
                    ...state,
                    loading: { ...state.loading, exporting: false },
                }));

                return { success: true, url: `#exported-${Date.now()}` };
            } catch (error) {
                set((state) => ({
                    ...state,
                    loading: { ...state.loading, exporting: false },
                    errors: { ...state.errors, export: 'Export failed' },
                }));

                return { success: false, error: 'Export failed' };
            }
        },

        /**
         * Get financial report by ID
         */
        getFinancialReportById: (id: string) => {
            return get().financialReports.find(report => report.id === id);
        },

        /**
         * Get sales analytics by ID
         */
        getSalesAnalyticsById: (id: string) => {
            return get().salesAnalytics.find(analytics => analytics.id === id);
        },

        /**
         * Get employee performance by ID
         */
        getEmployeePerformanceById: (id: string) => {
            return get().employeePerformance.find(emp => emp.employeeId === id);
        },

        /**
         * Delete financial report
         */
        deleteFinancialReport: (id: string) => {
            set((state) => ({
                ...state,
                financialReports: state.financialReports.filter(report => report.id !== id),
                activeReport: state.activeReport?.id === id ? null : state.activeReport,
            }));
        },

        /**
         * Delete sales analytics
         */
        deleteSalesAnalytics: (id: string) => {
            set((state) => ({
                ...state,
                salesAnalytics: state.salesAnalytics.filter(analytics => analytics.id !== id),
                activeReport: state.activeReport?.id === id ? null : state.activeReport,
            }));
        },

        /**
         * Set active report
         */
        setActiveReport: (report: any) => {
            set((state) => ({ ...state, activeReport: report }));
        },

        /**
         * Clear active report
         */
        clearActiveReport: () => {
            set((state) => ({ ...state, activeReport: null }));
        },

        /**
         * Set filters
         */
        setFilters: (filters: Partial<ReportFilter>) => {
            set((state) => ({
                ...state,
                filters: { ...state.filters, ...filters },
            }));
        },

        /**
         * Clear all errors
         */
        clearErrors: () => {
            set((state) => ({
                ...state,
                errors: { generation: null, export: null, fetch: null },
            }));
        },

        /**
         * Get reports by date range
         */
        getReportsByDateRange: (dateRange: DateRange) => {
            const state = get();
            const allReports = [
                ...state.financialReports,
                ...state.salesAnalytics,
            ];

            return allReports.filter(report =>
                report.period.startDate >= dateRange.startDate &&
                report.period.endDate <= dateRange.endDate
            );
        },

        /**
         * Get reports by type
         */
        getReportsByType: (type: ReportType) => {
            const state = get();

            switch (type) {
                case 'financial':
                case 'profit-loss':
                    return state.financialReports;
                case 'sales-analytics':
                    return state.salesAnalytics;
                case 'employee-performance':
                    return state.employeePerformance;
                default:
                    return [];
            }
        },

        /**
         * Refresh all reports
         */
        refreshReports: async () => {
            set((state) => ({
                ...state,
                loading: { ...state.loading, fetching: true },
            }));

            try {
                // Simulate refresh
                await new Promise(resolve => setTimeout(resolve, 1000));

                set((state) => ({
                    ...state,
                    loading: { ...state.loading, fetching: false },
                }));
            } catch (error) {
                set((state) => ({
                    ...state,
                    loading: { ...state.loading, fetching: false },
                    errors: {
                        ...state.errors,
                        fetch: error instanceof Error ? error.message : 'Failed to refresh reports'
                    },
                }));
            }
        },

        /**
         * Reset store to initial state
         */
        reset: () => {
            set(initialState);
        },
    }))
); 