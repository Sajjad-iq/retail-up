/**
 * Date range interface for filtering reports
 */
export interface DateRange {
    /** Start date for the report period */
    startDate: Date;
    /** End date for the report period */
    endDate: Date;
}

/**
 * Report filter base interface
 */
export interface ReportFilter {
    /** Date range for the report */
    dateRange: DateRange;
    /** Store/location filter */
    storeId?: string;
    /** Employee filter */
    employeeId?: string;
    /** Category filter */
    categoryId?: string;
}

/**
 * Report types enumeration
 */
export type ReportType =
    | 'financial'
    | 'sales-analytics'
    | 'product-performance'
    | 'inventory-analysis'
    | 'customer-analytics'
    | 'employee-performance'
    | 'profit-loss'
    | 'cash-flow';

/**
 * Report export format types
 */
export type ExportFormat = 'pdf' | 'excel' | 'csv' | 'json';

/**
 * Expense breakdown interface
 */
export interface ExpenseBreakdown {
    /** Expense category */
    category: string;
    /** Expense amount */
    amount: number;
    /** Percentage of total expenses */
    percentage: number;
    /** Expense description */
    description?: string;
}

/**
 * Revenue breakdown interface
 */
export interface RevenueBreakdown {
    /** Revenue category */
    category: string;
    /** Revenue amount */
    amount: number;
    /** Percentage of total revenue */
    percentage: number;
    /** Number of transactions */
    transactionCount: number;
}

/**
 * Financial report data interface
 */
export interface FinancialReport {
    /** Unique report identifier */
    id: string;
    /** Report title */
    title: string;
    /** Report period */
    period: DateRange;
    /** Total revenue */
    totalRevenue: number;
    /** Total expenses */
    totalExpenses: number;
    /** Net profit/loss */
    netProfit: number;
    /** Gross margin percentage */
    grossMargin: number;
    /** Cost of goods sold */
    costOfGoodsSold: number;
    /** Operating expenses breakdown */
    operatingExpenses: ExpenseBreakdown[];
    /** Revenue breakdown by category */
    revenueByCategory: RevenueBreakdown[];
    /** Report generation date */
    generatedAt: Date;
}

/**
 * Trend data interface
 */
export interface TrendData {
    /** Date point */
    date: string;
    /** Sales amount */
    sales: number;
    /** Number of transactions */
    transactions: number;
    /** Growth percentage from previous period */
    growthPercentage: number;
}

/**
 * Product performance interface
 */
export interface ProductPerformance {
    /** Product identifier */
    productId: string;
    /** Product name */
    productName: string;
    /** Product category */
    category: string;
    /** Total quantity sold */
    quantitySold: number;
    /** Total revenue generated */
    revenue: number;
    /** Profit margin */
    profitMargin: number;
    /** Inventory turnover rate */
    inventoryTurnover: number;
    /** ABC classification */
    abcClassification: 'A' | 'B' | 'C';
    /** Sales trend */
    trend: 'up' | 'down' | 'stable';
}

/**
 * Customer analytics interface
 */
export interface CustomerAnalytics {
    /** Total unique customers */
    totalCustomers: number;
    /** New customers in period */
    newCustomers: number;
    /** Returning customers */
    returningCustomers: number;
    /** Customer retention rate */
    retentionRate: number;
    /** Average customer lifetime value */
    averageLifetimeValue: number;
}

/**
 * Hourly sales data interface
 */
export interface HourlyData {
    /** Hour (0-23) */
    hour: number;
    /** Sales amount */
    sales: number;
    /** Number of transactions */
    transactions: number;
    /** Average transaction value */
    averageValue: number;
}

/**
 * Daily sales data interface
 */
export interface DailyData {
    /** Day of week (0-6, Sunday=0) */
    dayOfWeek: number;
    /** Day name */
    dayName: string;
    /** Sales amount */
    sales: number;
    /** Number of transactions */
    transactions: number;
    /** Average transaction value */
    averageValue: number;
}

/**
 * Payment method data interface
 */
export interface PaymentMethodData {
    /** Payment method name */
    method: string;
    /** Transaction count */
    transactionCount: number;
    /** Total amount */
    amount: number;
    /** Percentage of total */
    percentage: number;
}

/**
 * Sales analytics data interface
 */
export interface SalesAnalytics {
    /** Unique analytics identifier */
    id: string;
    /** Analytics period */
    period: DateRange;
    /** Total sales amount */
    totalSales: number;
    /** Total transactions */
    totalTransactions: number;
    /** Average transaction value */
    averageTransactionValue: number;
    /** Sales trend data */
    salesTrend: TrendData[];
    /** Top selling products */
    topProducts: ProductPerformance[];
    /** Sales by hour of day */
    salesByHour: HourlyData[];
    /** Sales by day of week */
    salesByDayOfWeek: DailyData[];
    /** Customer analytics */
    customerAnalytics: CustomerAnalytics;
    /** Payment method breakdown */
    paymentMethods: PaymentMethodData[];
    /** Report generation date */
    generatedAt: Date;
}

/**
 * Employee performance interface
 */
export interface EmployeePerformance {
    /** Employee identifier */
    employeeId: string;
    /** Employee name */
    employeeName: string;
    /** Employee role */
    role: string;
    /** Total sales amount */
    totalSales: number;
    /** Number of transactions */
    transactionCount: number;
    /** Average transaction value */
    averageTransactionValue: number;
    /** Sales per hour */
    salesPerHour: number;
    /** Customer satisfaction score */
    customerSatisfaction?: number;
    /** Upselling success rate */
    upsellRate: number;
    /** Performance trend */
    trend: 'improving' | 'declining' | 'stable';
}

/**
 * Report export request interface
 */
export interface ExportRequest {
    /** Report type */
    reportType: ReportType;
    /** Export format */
    format: ExportFormat;
    /** Report filters */
    filters: ReportFilter;
    /** Include charts */
    includeCharts?: boolean;
    /** Include raw data */
    includeRawData?: boolean;
}

/**
 * Reporting store state interface
 */
export interface ReportingState {
    /** Financial reports */
    financialReports: FinancialReport[];
    /** Sales analytics */
    salesAnalytics: SalesAnalytics[];
    /** Employee performance data */
    employeePerformance: EmployeePerformance[];
    /** Current active report */
    activeReport: any | null;
    /** Loading states */
    loading: {
        generating: boolean;
        exporting: boolean;
        fetching: boolean;
    };
    /** Error states */
    errors: {
        generation: string | null;
        export: string | null;
        fetch: string | null;
    };
    /** Report filters */
    filters: ReportFilter;
}

/**
 * Inventory analysis interface
 */
export interface InventoryAnalysis {
    /** Unique analysis identifier */
    id: string;
    /** Analysis period */
    period: DateRange;
    /** Total inventory value */
    totalInventoryValue: number;
    /** Inventory turnover ratio */
    inventoryTurnoverRatio: number;
    /** Days of inventory outstanding */
    daysInventoryOutstanding: number;
    /** ABC analysis results */
    abcAnalysis: ABCAnalysis;
    /** Slow moving items */
    slowMovingItems: SlowMovingItem[];
    /** Overstock items */
    overstockItems: OverstockItem[];
    /** Stockout analysis */
    stockoutAnalysis: StockoutAnalysis;
    /** Report generation date */
    generatedAt: Date;
}

/**
 * ABC analysis interface
 */
export interface ABCAnalysis {
    /** Class A items (high value) */
    classA: {
        itemCount: number;
        valuePercentage: number;
        items: ProductPerformance[];
    };
    /** Class B items (medium value) */
    classB: {
        itemCount: number;
        valuePercentage: number;
        items: ProductPerformance[];
    };
    /** Class C items (low value) */
    classC: {
        itemCount: number;
        valuePercentage: number;
        items: ProductPerformance[];
    };
}

/**
 * Slow moving item interface
 */
export interface SlowMovingItem {
    /** Product identifier */
    productId: string;
    /** Product name */
    productName: string;
    /** Current stock quantity */
    currentStock: number;
    /** Days since last sale */
    daysSinceLastSale: number;
    /** Monthly turnover rate */
    monthlyTurnover: number;
    /** Recommended action */
    recommendedAction: 'discount' | 'return' | 'liquidate';
}

/**
 * Overstock item interface
 */
export interface OverstockItem {
    /** Product identifier */
    productId: string;
    /** Product name */
    productName: string;
    /** Current stock quantity */
    currentStock: number;
    /** Optimal stock level */
    optimalStock: number;
    /** Excess quantity */
    excessQuantity: number;
    /** Tied up capital */
    tiedUpCapital: number;
}

/**
 * Stockout analysis interface
 */
export interface StockoutAnalysis {
    /** Total stockout incidents */
    totalStockouts: number;
    /** Lost sales value */
    lostSalesValue: number;
    /** Most frequent stockouts */
    frequentStockouts: StockoutIncident[];
    /** Stockout prevention recommendations */
    recommendations: string[];
}

/**
 * Stockout incident interface
 */
export interface StockoutIncident {
    /** Product identifier */
    productId: string;
    /** Product name */
    productName: string;
    /** Number of stockout days */
    stockoutDays: number;
    /** Estimated lost sales */
    estimatedLostSales: number;
    /** Last stockout date */
    lastStockoutDate: Date;
}

/**
 * Cash flow report interface
 */
export interface CashFlowReport {
    /** Unique report identifier */
    id: string;
    /** Report period */
    period: DateRange;
    /** Opening cash balance */
    openingBalance: number;
    /** Cash inflows */
    cashInflows: CashFlowItem[];
    /** Cash outflows */
    cashOutflows: CashFlowItem[];
    /** Net cash flow */
    netCashFlow: number;
    /** Closing cash balance */
    closingBalance: number;
    /** Cash flow by period */
    periodBreakdown: CashFlowPeriod[];
    /** Report generation date */
    generatedAt: Date;
}

/**
 * Cash flow item interface
 */
export interface CashFlowItem {
    /** Item category */
    category: string;
    /** Item description */
    description: string;
    /** Amount */
    amount: number;
    /** Date */
    date: Date;
}

/**
 * Cash flow period interface
 */
export interface CashFlowPeriod {
    /** Period start date */
    startDate: Date;
    /** Period end date */
    endDate: Date;
    /** Net cash flow for period */
    netCashFlow: number;
    /** Cumulative cash flow */
    cumulativeCashFlow: number;
}

/**
 * Date range preset interface
 */
export interface DateRangePreset {
    /** Preset label */
    label: string;
    /** Preset value */
    value: string;
    /** Start date */
    startDate: Date;
    /** End date */
    endDate: Date;
} 