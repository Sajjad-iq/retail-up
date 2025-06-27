import { useMemo } from 'react';
import { useReportingStore } from '../store/reporting-store';
import type {
    FinancialReport,
    SalesAnalytics,
    EmployeePerformance,
    ReportFilter,
    ExportRequest,
    ReportType,
    DateRange
} from '../types/reporting';

/**
 * Custom hook for general reporting operations
 * Provides core reporting functionality and state
 */
export function useReporting() {
    const {
        activeReport,
        loading,
        errors,
        filters,
        setActiveReport,
        clearActiveReport,
        setFilters,
        clearErrors,
        reset
    } = useReportingStore();

    const computedValues = useMemo(() => ({
        isGenerating: loading.generating,
        isExporting: loading.exporting,
        isFetching: loading.fetching,
        hasGenerationError: !!errors.generation,
        hasExportError: !!errors.export,
        hasFetchError: !!errors.fetch,
        hasActiveReport: !!activeReport,
        currentFilters: filters
    }), [loading, errors, activeReport, filters]);

    return {
        // State
        activeReport,
        loading,
        errors,
        filters,
        ...computedValues,

        // Actions
        setActiveReport,
        clearActiveReport,
        setFilters,
        clearErrors,
        reset,
    };
}

/**
 * Custom hook for financial reporting operations
 * Provides financial report state and management functionality
 */
export function useFinancialReports() {
    const {
        financialReports,
        loading,
        errors,
        generateFinancialReport,
        deleteFinancialReport,
        getFinancialReportById
    } = useReportingStore();

    const computedValues = useMemo(() => ({
        totalReports: financialReports.length,
        latestReport: financialReports[financialReports.length - 1],
        recentReports: financialReports.slice(-5),
        isGenerating: loading.generating,
        generationError: errors.generation
    }), [financialReports, loading.generating, errors.generation]);

    const generateReport = async (filters: ReportFilter) => {
        try {
            const report = await generateFinancialReport(filters);
            return { success: true, report };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to generate financial report'
            };
        }
    };

    const getReportById = (id: string) => {
        return getFinancialReportById(id);
    };

    const deleteReport = (id: string) => {
        deleteFinancialReport(id);
    };

    const getReportsByDateRange = (dateRange: DateRange) => {
        return financialReports.filter(report =>
            report.period.startDate >= dateRange.startDate &&
            report.period.endDate <= dateRange.endDate
        );
    };

    return {
        // State
        financialReports,
        ...computedValues,

        // Actions
        generateReport,
        getReportById,
        deleteReport,
        getReportsByDateRange,
    };
}

/**
 * Custom hook for sales analytics operations
 * Provides sales analytics state and management functionality
 */
export function useSalesAnalytics() {
    const {
        salesAnalytics,
        loading,
        errors,
        generateSalesAnalytics,
        deleteSalesAnalytics,
        getSalesAnalyticsById
    } = useReportingStore();

    const computedValues = useMemo(() => ({
        totalAnalytics: salesAnalytics.length,
        latestAnalytics: salesAnalytics[salesAnalytics.length - 1],
        recentAnalytics: salesAnalytics.slice(-5),
        isGenerating: loading.generating,
        generationError: errors.generation
    }), [salesAnalytics, loading.generating, errors.generation]);

    const generateAnalytics = async (filters: ReportFilter) => {
        try {
            const analytics = await generateSalesAnalytics(filters);
            return { success: true, analytics };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to generate sales analytics'
            };
        }
    };

    const getAnalyticsById = (id: string) => {
        return getSalesAnalyticsById(id);
    };

    const deleteAnalytics = (id: string) => {
        deleteSalesAnalytics(id);
    };

    const getAnalyticsByDateRange = (dateRange: DateRange) => {
        return salesAnalytics.filter(analytics =>
            analytics.period.startDate >= dateRange.startDate &&
            analytics.period.endDate <= dateRange.endDate
        );
    };

    return {
        // State
        salesAnalytics,
        ...computedValues,

        // Actions
        generateAnalytics,
        getAnalyticsById,
        deleteAnalytics,
        getAnalyticsByDateRange,
    };
}

/**
 * Custom hook for employee performance operations
 * Provides employee performance state and management functionality
 */
export function useEmployeePerformance() {
    const {
        employeePerformance,
        loading,
        errors,
        generateEmployeePerformance,
        getEmployeePerformanceById
    } = useReportingStore();

    const computedValues = useMemo(() => ({
        totalEmployees: employeePerformance.length,
        topPerformers: employeePerformance
            .sort((a, b) => b.totalSales - a.totalSales)
            .slice(0, 3),
        improvingEmployees: employeePerformance.filter(emp => emp.trend === 'improving'),
        decliningEmployees: employeePerformance.filter(emp => emp.trend === 'declining'),
        averageSales: employeePerformance.length > 0
            ? employeePerformance.reduce((sum, emp) => sum + emp.totalSales, 0) / employeePerformance.length
            : 0,
        isGenerating: loading.generating,
        generationError: errors.generation
    }), [employeePerformance, loading.generating, errors.generation]);

    const generatePerformance = async (filters: ReportFilter) => {
        try {
            const performance = await generateEmployeePerformance(filters);
            return { success: true, performance };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to generate employee performance'
            };
        }
    };

    const getPerformanceById = (id: string) => {
        return getEmployeePerformanceById(id);
    };

    const getPerformanceByRole = (role: string) => {
        return employeePerformance.filter(emp => emp.role === role);
    };

    const getPerformanceByTrend = (trend: 'improving' | 'declining' | 'stable') => {
        return employeePerformance.filter(emp => emp.trend === trend);
    };

    return {
        // State
        employeePerformance,
        ...computedValues,

        // Actions
        generatePerformance,
        getPerformanceById,
        getPerformanceByRole,
        getPerformanceByTrend,
    };
}

/**
 * Custom hook for report export operations
 * Provides export functionality for all report types
 */
export function useReportExport() {
    const {
        loading,
        errors,
        exportReport
    } = useReportingStore();

    const computedValues = useMemo(() => ({
        isExporting: loading.exporting,
        exportError: errors.export,
        canExport: !loading.exporting
    }), [loading.exporting, errors.export]);

    const exportReportWithValidation = async (request: ExportRequest) => {
        try {
            const result = await exportReport(request);
            return result;
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Export failed'
            };
        }
    };

    const downloadReport = (url: string, filename: string) => {
        // Create download link
        const link = document.createElement('a');
        link.href = url;
        link.download = filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    };

    const shareReport = async (url: string, title: string) => {
        if (navigator.share) {
            try {
                await navigator.share({
                    title,
                    url
                });
                return { success: true };
            } catch (error) {
                return { success: false, error: 'Sharing failed' };
            }
        } else {
            // Fallback: copy to clipboard
            try {
                await navigator.clipboard.writeText(url);
                return { success: true, message: 'Link copied to clipboard' };
            } catch (error) {
                return { success: false, error: 'Failed to copy link' };
            }
        }
    };

    return {
        // State
        ...computedValues,

        // Actions
        exportReport: exportReportWithValidation,
        downloadReport,
        shareReport,
    };
}

/**
 * Custom hook for report dashboard operations
 * Provides dashboard-specific functionality and aggregated data
 */
export function useReportDashboard() {
    const {
        financialReports,
        salesAnalytics,
        employeePerformance,
        loading,
        filters
    } = useReportingStore();

    const dashboardMetrics = useMemo(() => {
        const latestFinancial = financialReports[financialReports.length - 1];
        const latestSales = salesAnalytics[salesAnalytics.length - 1];

        return {
            totalRevenue: latestFinancial?.totalRevenue || 0,
            totalProfit: latestFinancial?.netProfit || 0,
            totalSales: latestSales?.totalSales || 0,
            totalTransactions: latestSales?.totalTransactions || 0,
            totalEmployees: employeePerformance.length,
            averageTransactionValue: latestSales?.averageTransactionValue || 0,
            profitMargin: latestFinancial
                ? ((latestFinancial.netProfit / latestFinancial.totalRevenue) * 100)
                : 0,
            topEmployee: employeePerformance.length > 0
                ? employeePerformance.reduce((top, emp) =>
                    emp.totalSales > top.totalSales ? emp : top
                )
                : null
        };
    }, [financialReports, salesAnalytics, employeePerformance]);

    const recentActivity = useMemo(() => {
        const activities = [
            ...financialReports.map(report => ({
                type: 'financial' as const,
                title: report.title,
                date: report.generatedAt,
                id: report.id
            })),
            ...salesAnalytics.map(analytics => ({
                type: 'sales' as const,
                title: `Sales Analytics - ${analytics.period.startDate.toLocaleDateString()}`,
                date: analytics.generatedAt,
                id: analytics.id
            }))
        ];

        return activities
            .sort((a, b) => b.date.getTime() - a.date.getTime())
            .slice(0, 10);
    }, [financialReports, salesAnalytics]);

    const isLoadingAny = useMemo(() => {
        return loading.generating || loading.exporting || loading.fetching;
    }, [loading]);

    return {
        // Computed metrics
        dashboardMetrics,
        recentActivity,

        // State
        isLoading: isLoadingAny,
        currentPeriod: filters.dateRange,

        // Counts
        totalReports: financialReports.length + salesAnalytics.length,
        totalFinancialReports: financialReports.length,
        totalSalesAnalytics: salesAnalytics.length,
    };
}