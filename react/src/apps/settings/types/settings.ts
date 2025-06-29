/**
 * Settings type definitions for the retail management application
 * Provides comprehensive configuration options for all aspects of the business
 */

/**
 * Main application settings interface containing all configuration categories
 */
export interface AppSettings {
    business: BusinessSettings;
    pos: POSSettings;
    inventory: InventorySettings;
    payments: PaymentSettings;
    staff: StaffSettings;
    customers: CustomerSettings;
    notifications: NotificationSettings;
    reports: ReportSettings;
    appearance: AppearanceSettings;
    integrations: IntegrationSettings;
    system: SystemSettings;
}

/**
 * Business and store information settings
 */
export interface BusinessSettings {
    // Store Information
    businessName: string;
    storeName: string;
    address: string;
    phone: string;
    email: string;
    website: string;

    // Business Hours
    businessHours: BusinessHours;

    // Regional Settings
    currency: Currency;
    timezone: string;
    language: Language;
}

/**
 * Business operating hours configuration
 */
export interface BusinessHours {
    monday: DaySchedule;
    tuesday: DaySchedule;
    wednesday: DaySchedule;
    thursday: DaySchedule;
    friday: DaySchedule;
    saturday: DaySchedule;
    sunday: DaySchedule;
}

/**
 * Daily schedule configuration
 */
export interface DaySchedule {
    isOpen: boolean;
    openTime: string;
    closeTime: string;
}

/**
 * Point of Sale system settings
 */
export interface POSSettings {
    // Receipt Settings
    receiptSettings: ReceiptSettings;

    // Hardware Settings
    hardware: HardwareSettings;

    // Interface Settings
    interface: POSInterfaceSettings;
}

/**
 * Receipt configuration and printing options
 */
export interface ReceiptSettings {
    autoPrint: boolean;
    emailReceipts: boolean;
    footerText: string;
    logoUrl: string;
    includeBarcode: boolean;
    paperSize: 'thermal' | 'standard';
}

/**
 * Hardware device configuration
 */
export interface HardwareSettings {
    barcodeScanner: boolean;
    cashDrawer: boolean;
    customerDisplay: boolean;
    cardReader: boolean;
    weightScale: boolean;
    printerPort: string;
}

/**
 * POS interface preferences
 */
export interface POSInterfaceSettings {
    gridSize: 'small' | 'medium' | 'large';
    showProductImages: boolean;
    enableQuickKeys: boolean;
    soundEffects: boolean;
}

/**
 * Inventory management settings
 */
export interface InventorySettings {
    // Stock Management
    stockTracking: StockTrackingSettings;

    // Product Management
    productSettings: ProductSettings;

    // Supplier Settings
    supplierSettings: SupplierSettings;
}

/**
 * Stock tracking and alert configuration
 */
export interface StockTrackingSettings {
    autoTrackInventory: boolean;
    lowStockAlerts: boolean;
    lowStockThreshold: number;
    criticalStockThreshold: number;
    outOfStockAlerts: boolean;
    trackSerialNumbers: boolean;
    trackExpirationDates: boolean;
}

/**
 * Product creation and management settings
 */
export interface ProductSettings {
    autoGenerateSKU: boolean;
    requireProductImages: boolean;
    defaultCategory: string;
    enableVariants: boolean;
    requireBarcode: boolean;
    defaultTaxRate: number;
}

/**
 * Supplier management configuration
 */
export interface SupplierSettings {
    autoReorder: boolean;
    reorderThreshold: number;
    defaultLeadTime: number;
    requirePurchaseOrders: boolean;
}

/**
 * Payment processing and financial settings
 */
export interface PaymentSettings {
    // Payment Methods
    paymentMethods: PaymentMethodSettings;

    // Tax Configuration
    taxSettings: TaxSettings;

    // Financial Settings
    financialSettings: FinancialSettings;
}

/**
 * Available payment method configuration
 */
export interface PaymentMethodSettings {
    cash: boolean;
    creditDebit: boolean;
    digitalWallets: boolean;
    storeCredit: boolean;
    checks: boolean;
    layaway: boolean;
    giftCards: boolean;
}

/**
 * Tax calculation and reporting settings
 */
export interface TaxSettings {
    salesTaxRate: number;
    taxIdNumber: string;
    taxInclusivePricing: boolean;
    automaticTaxCalculation: boolean;
    taxExemptCategories: string[];
    multipleJurisdictions: boolean;
}

/**
 * Financial reporting and accounting settings
 */
export interface FinancialSettings {
    baseCurrency: Currency;
    accountingMethod: 'cash' | 'accrual';
    fiscalYearStart: string;
    automaticDeposits: boolean;
    tipHandling: 'cash' | 'credit' | 'split';
}

/**
 * Staff management and security settings
 */
export interface StaffSettings {
    // User Management
    userManagement: UserManagementSettings;

    // Security Policies
    security: SecuritySettings;

    // Access Control
    accessControl: AccessControlSettings;
}

/**
 * User account and role management
 */
export interface UserManagementSettings {
    requireEmployeePIN: boolean;
    trackEmployeeSales: boolean;
    allowManagerOverrides: boolean;
    employeeDiscounts: boolean;
    timeClock: boolean;
    performanceTracking: boolean;
}

/**
 * Security policy configuration
 */
export interface SecuritySettings {
    sessionTimeout: number;
    passwordPolicy: PasswordPolicy;
    auditLogging: boolean;
    twoFactorAuth: boolean;
    dataEncryption: boolean;
    loginAttempts: number;
}

/**
 * Password security requirements
 */
export interface PasswordPolicy {
    minLength: number;
    requireUppercase: boolean;
    requireNumbers: boolean;
    requireSymbols: boolean;
    expirationDays: number;
}

/**
 * Role-based access control settings
 */
export interface AccessControlSettings {
    roles: UserRole[];
    permissions: Permission[];
    departmentAccess: boolean;
    timeBasedAccess: boolean;
}

/**
 * Customer management and loyalty settings
 */
export interface CustomerSettings {
    // Customer Data
    customerData: CustomerDataSettings;

    // Loyalty Program
    loyaltyProgram: LoyaltyProgramSettings;

    // Marketing
    marketing: MarketingSettings;
}

/**
 * Customer information collection settings
 */
export interface CustomerDataSettings {
    collectEmails: boolean;
    collectPhones: boolean;
    collectAddresses: boolean;
    requireCustomerInfo: boolean;
    privacyCompliance: boolean;
    dataRetentionPeriod: number;
}

/**
 * Loyalty program configuration
 */
export interface LoyaltyProgramSettings {
    enabled: boolean;
    pointsPerDollar: number;
    dollarValuePerPoint: number;
    tierSystem: boolean;
    pointExpiration: number;
    welcomeBonus: number;
    birthdayRewards: boolean;
}

/**
 * Customer marketing preferences
 */
export interface MarketingSettings {
    emailMarketing: boolean;
    smsMarketing: boolean;
    promotionalOffers: boolean;
    surveyRequests: boolean;
    reviewRequests: boolean;
}

/**
 * Notification and alert settings
 */
export interface NotificationSettings {
    // Delivery Methods
    email: boolean;
    push: boolean;
    sms: boolean;
    dashboard: boolean;

    // Alert Types
    alerts: AlertSettings;

    // Notification Schedule
    schedule: NotificationSchedule;
}

/**
 * Specific alert type configuration
 */
export interface AlertSettings {
    lowStock: boolean;
    dailySummary: boolean;
    systemErrors: boolean;
    newOrders: boolean;
    paymentFailures: boolean;
    securityAlerts: boolean;
    maintenanceAlerts: boolean;
    performanceAlerts: boolean;
}

/**
 * Notification timing and frequency
 */
export interface NotificationSchedule {
    immediateAlerts: string[];
    dailyDigest: boolean;
    weeklyReports: boolean;
    monthlyReports: boolean;
    quietHours: {
        start: string;
        end: string;
    };
}

/**
 * Reporting and analytics settings
 */
export interface ReportSettings {
    // Report Generation
    generation: ReportGenerationSettings;

    // Data Analytics
    analytics: AnalyticsSettings;

    // Export Options
    exportOptions: ExportSettings;
}

/**
 * Automated report generation settings
 */
export interface ReportGenerationSettings {
    autoGenerateDaily: boolean;
    autoGenerateWeekly: boolean;
    autoGenerateMonthly: boolean;
    defaultPeriod: 'today' | 'week' | 'month' | 'quarter' | 'year';
    reportEmail: string;
    includeCharts: boolean;
}

/**
 * Analytics and insights configuration
 */
export interface AnalyticsSettings {
    trackCustomerBehavior: boolean;
    salesForecasting: boolean;
    inventoryAnalytics: boolean;
    employeePerformance: boolean;
    profitAnalysis: boolean;
    dataRetention: number;
}

/**
 * Report export and sharing options
 */
export interface ExportSettings {
    allowedFormats: ExportFormat[];
    emailReports: boolean;
    cloudStorage: boolean;
    automaticBackup: boolean;
    shareWithAccountant: boolean;
}

/**
 * Application appearance and theme settings
 */
export interface AppearanceSettings {
    // Theme Configuration
    theme: 'light' | 'dark' | 'system';
    colorScheme: ColorScheme;

    // Layout Options
    layout: LayoutSettings;

    // Display Preferences
    display: DisplaySettings;
}

/**
 * Color scheme customization
 */
export interface ColorScheme {
    primary: string;
    secondary: string;
    accent: string;
    background: string;
    surface: string;
}

/**
 * Layout and spacing preferences
 */
export interface LayoutSettings {
    sidebarWidth: 'narrow' | 'standard' | 'wide';
    compactMode: boolean;
    gridDensity: 'comfortable' | 'standard' | 'compact';
    showSidebar: boolean;
}

/**
 * Display and typography settings
 */
export interface DisplaySettings {
    fontSize: 'small' | 'medium' | 'large';
    fontFamily: string;
    animations: boolean;
    reducedMotion: boolean;
    highContrast: boolean;
}

/**
 * Third-party integrations and APIs
 */
export interface IntegrationSettings {
    // Accounting Integration
    accounting: AccountingIntegration;

    // E-commerce Integration
    ecommerce: EcommerceIntegration;

    // Marketing Integration
    marketing: MarketingIntegration;

    // Payment Processing
    paymentProcessors: PaymentProcessorSettings;
}

/**
 * Accounting software integration
 */
export interface AccountingIntegration {
    provider: 'quickbooks' | 'xero' | 'sage' | 'none';
    apiKey: string;
    syncFrequency: 'realtime' | 'hourly' | 'daily';
    syncSales: boolean;
    syncInventory: boolean;
    syncCustomers: boolean;
}

/**
 * E-commerce platform integration
 */
export interface EcommerceIntegration {
    provider: 'shopify' | 'woocommerce' | 'magento' | 'none';
    storeUrl: string;
    apiCredentials: string;
    syncInventory: boolean;
    syncOrders: boolean;
    autoFulfillment: boolean;
}

/**
 * Marketing platform integration
 */
export interface MarketingIntegration {
    emailProvider: 'mailchimp' | 'constant_contact' | 'klaviyo' | 'none';
    smsProvider: 'twilio' | 'textmagic' | 'none';
    socialMedia: SocialMediaIntegration;
    analytics: AnalyticsIntegration;
}

/**
 * Social media platform connections
 */
export interface SocialMediaIntegration {
    facebook: boolean;
    instagram: boolean;
    twitter: boolean;
    googleMyBusiness: boolean;
}

/**
 * Analytics platform integration
 */
export interface AnalyticsIntegration {
    googleAnalytics: boolean;
    facebookPixel: boolean;
    customAnalytics: boolean;
}

/**
 * Payment processor configuration
 */
export interface PaymentProcessorSettings {
    primaryProcessor: PaymentProcessor;
    backupProcessor: PaymentProcessor;
    fees: ProcessingFees;
    limits: ProcessingLimits;
}

/**
 * Individual payment processor settings
 */
export interface PaymentProcessor {
    provider: 'stripe' | 'square' | 'paypal' | 'clover' | 'none';
    merchantId: string;
    apiKey: string;
    isActive: boolean;
    supportedMethods: string[];
}

/**
 * Payment processing fee structure
 */
export interface ProcessingFees {
    creditCardRate: number;
    debitCardRate: number;
    transactionFee: number;
    monthlyFee: number;
}

/**
 * Transaction limit settings
 */
export interface ProcessingLimits {
    dailyLimit: number;
    transactionLimit: number;
    monthlyLimit: number;
    requireSignature: number;
}

/**
 * System administration and maintenance settings
 */
export interface SystemSettings {
    // Data Management
    dataManagement: DataManagementSettings;

    // Backup Configuration
    backup: BackupSettings;

    // System Maintenance
    maintenance: MaintenanceSettings;

    // Performance Settings
    performance: PerformanceSettings;
}

/**
 * Data retention and management policies
 */
export interface DataManagementSettings {
    automaticBackup: boolean;
    dataRetentionPeriod: number;
    cloudSync: boolean;
    dataExportSchedule: string;
    archiveOldData: boolean;
    gdprCompliance: boolean;
}

/**
 * Backup strategy and configuration
 */
export interface BackupSettings {
    frequency: 'hourly' | 'daily' | 'weekly';
    retention: number;
    cloudProvider: 'aws' | 'google' | 'azure' | 'local';
    encryption: boolean;
    compression: boolean;
    verifyIntegrity: boolean;
}

/**
 * System maintenance and updates
 */
export interface MaintenanceSettings {
    autoUpdates: boolean;
    maintenanceWindow: string;
    updateChannel: 'stable' | 'beta' | 'alpha';
    notifyBeforeUpdates: boolean;
    rollbackEnabled: boolean;
}

/**
 * Performance optimization settings
 */
export interface PerformanceSettings {
    cacheEnabled: boolean;
    imageCaching: boolean;
    databaseOptimization: boolean;
    logLevel: 'error' | 'warn' | 'info' | 'debug';
    maxConcurrentUsers: number;
}

// ============================================================================
// ENUMS AND UTILITY TYPES
// ============================================================================

/**
 * Supported currencies
 */
export type Currency = 'USD' | 'EUR' | 'GBP' | 'CAD' | 'AUD' | 'JPY';

/**
 * Supported languages
 */
export type Language = 'en' | 'es' | 'fr' | 'de' | 'it' | 'pt';

/**
 * User role definitions
 */
export interface UserRole {
    id: string;
    name: string;
    description: string;
    permissions: string[];
}

/**
 * Permission definition
 */
export interface Permission {
    id: string;
    name: string;
    description: string;
    category: string;
}

/**
 * Export format options
 */
export type ExportFormat = 'pdf' | 'excel' | 'csv' | 'json';

/**
 * Settings category identifiers
 */
export type SettingsCategory =
    | 'business'
    | 'pos'
    | 'inventory'
    | 'payments'
    | 'staff'
    | 'customers'
    | 'notifications'
    | 'reports'
    | 'appearance'
    | 'integrations'
    | 'system';

/**
 * Settings update payload
 */
export interface SettingsUpdatePayload {
    category: SettingsCategory;
    settings: Partial<AppSettings[SettingsCategory]>;
}

/**
 * Settings validation result
 */
export interface SettingsValidationResult {
    isValid: boolean;
    errors: SettingsValidationError[];
    warnings: SettingsValidationWarning[];
}

/**
 * Settings validation error
 */
export interface SettingsValidationError {
    field: string;
    message: string;
    code: string;
}

/**
 * Settings validation warning
 */
export interface SettingsValidationWarning {
    field: string;
    message: string;
    suggestion?: string;
}

/**
 * Settings import/export format
 */
export interface SettingsExportData {
    version: string;
    exportDate: string;
    settings: AppSettings;
    metadata: {
        appVersion: string;
        storeName: string;
        totalCategories: number;
    };
} 