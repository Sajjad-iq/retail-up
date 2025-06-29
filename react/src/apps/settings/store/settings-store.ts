import { create } from 'zustand';
import { devtools, persist } from 'zustand/middleware';
import type {
    AppSettings,
    SettingsCategory,
    BusinessSettings,
    POSSettings,
    InventorySettings,
    PaymentSettings,
    StaffSettings,
    CustomerSettings,
    NotificationSettings,
    ReportSettings,
    AppearanceSettings,
    IntegrationSettings,
    SystemSettings,
    SettingsUpdatePayload,
    SettingsValidationResult
} from '../types/settings';
import {
    generateSettingsId,
    getDefaultSettings,
    validateSettingsData
} from '../lib/utils/settings-utils';

/**
 * Settings activity log entry
 */
interface SettingsActivity {
    id: string;
    category: SettingsCategory;
    action: 'created' | 'updated' | 'deleted' | 'reset' | 'imported' | 'exported';
    previousValue?: any;
    newValue?: any;
    timestamp: Date;
    details?: string;
}

/**
 * Default business settings
 */
const defaultBusinessSettings: BusinessSettings = {
    businessName: 'My Retail Store',
    storeName: 'Main Store',
    address: '123 Main Street\nAnytown, State 12345\nUnited States',
    phone: '+1 (555) 123-4567',
    email: 'info@mystore.com',
    website: 'https://mystore.com',
    businessHours: {
        monday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        tuesday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        wednesday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        thursday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        friday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        saturday: { isOpen: true, openTime: '10:00', closeTime: '16:00' },
        sunday: { isOpen: false, openTime: '00:00', closeTime: '00:00' }
    },
    currency: 'USD',
    timezone: 'America/New_York',
    language: 'en'
};

/**
 * Default POS settings
 */
const defaultPOSSettings: POSSettings = {
    receiptSettings: {
        autoPrint: true,
        emailReceipts: true,
        footerText: 'Thank you for your business!\nVisit us again soon.',
        logoUrl: '',
        includeBarcode: true,
        paperSize: 'thermal'
    },
    hardware: {
        barcodeScanner: true,
        cashDrawer: true,
        customerDisplay: false,
        cardReader: true,
        weightScale: false,
        printerPort: 'COM1'
    },
    interface: {
        gridSize: 'medium',
        showProductImages: true,
        enableQuickKeys: true,
        soundEffects: true
    }
};

/**
 * Default inventory settings
 */
const defaultInventorySettings: InventorySettings = {
    stockTracking: {
        autoTrackInventory: true,
        lowStockAlerts: true,
        lowStockThreshold: 10,
        criticalStockThreshold: 3,
        outOfStockAlerts: true,
        trackSerialNumbers: false,
        trackExpirationDates: false
    },
    productSettings: {
        autoGenerateSKU: true,
        requireProductImages: false,
        defaultCategory: 'general',
        enableVariants: true,
        requireBarcode: false,
        defaultTaxRate: 8.25
    },
    supplierSettings: {
        autoReorder: false,
        reorderThreshold: 5,
        defaultLeadTime: 7,
        requirePurchaseOrders: false
    }
};

/**
 * Default payment settings
 */
const defaultPaymentSettings: PaymentSettings = {
    paymentMethods: {
        cash: true,
        creditDebit: true,
        digitalWallets: false,
        storeCredit: true,
        checks: false,
        layaway: false,
        giftCards: true
    },
    taxSettings: {
        salesTaxRate: 8.25,
        taxIdNumber: '',
        taxInclusivePricing: false,
        automaticTaxCalculation: true,
        taxExemptCategories: [],
        multipleJurisdictions: false
    },
    financialSettings: {
        baseCurrency: 'USD',
        accountingMethod: 'cash',
        fiscalYearStart: '01-01',
        automaticDeposits: false,
        tipHandling: 'cash'
    }
};

/**
 * Default staff settings
 */
const defaultStaffSettings: StaffSettings = {
    userManagement: {
        requireEmployeePIN: true,
        trackEmployeeSales: true,
        allowManagerOverrides: true,
        employeeDiscounts: false,
        timeClock: false,
        performanceTracking: true
    },
    security: {
        sessionTimeout: 30,
        passwordPolicy: {
            minLength: 8,
            requireUppercase: true,
            requireNumbers: true,
            requireSymbols: false,
            expirationDays: 90
        },
        auditLogging: true,
        twoFactorAuth: false,
        dataEncryption: true,
        loginAttempts: 5
    },
    accessControl: {
        roles: [],
        permissions: [],
        departmentAccess: false,
        timeBasedAccess: false
    }
};

/**
 * Default customer settings
 */
const defaultCustomerSettings: CustomerSettings = {
    customerData: {
        collectEmails: true,
        collectPhones: false,
        collectAddresses: false,
        requireCustomerInfo: false,
        privacyCompliance: true,
        dataRetentionPeriod: 365
    },
    loyaltyProgram: {
        enabled: true,
        pointsPerDollar: 1,
        dollarValuePerPoint: 0.01,
        tierSystem: false,
        pointExpiration: 365,
        welcomeBonus: 100,
        birthdayRewards: true
    },
    marketing: {
        emailMarketing: true,
        smsMarketing: false,
        promotionalOffers: true,
        surveyRequests: false,
        reviewRequests: true
    }
};

/**
 * Default notification settings
 */
const defaultNotificationSettings: NotificationSettings = {
    email: true,
    push: true,
    sms: false,
    dashboard: true,
    alerts: {
        lowStock: true,
        dailySummary: true,
        systemErrors: true,
        newOrders: true,
        paymentFailures: true,
        securityAlerts: true,
        maintenanceAlerts: false,
        performanceAlerts: false
    },
    schedule: {
        immediateAlerts: ['systemErrors', 'securityAlerts', 'paymentFailures'],
        dailyDigest: true,
        weeklyReports: false,
        monthlyReports: true,
        quietHours: {
            start: '22:00',
            end: '08:00'
        }
    }
};

/**
 * Default report settings
 */
const defaultReportSettings: ReportSettings = {
    generation: {
        autoGenerateDaily: true,
        autoGenerateWeekly: false,
        autoGenerateMonthly: true,
        defaultPeriod: 'month',
        reportEmail: '',
        includeCharts: true
    },
    analytics: {
        trackCustomerBehavior: true,
        salesForecasting: false,
        inventoryAnalytics: true,
        employeePerformance: true,
        profitAnalysis: true,
        dataRetention: 365
    },
    exportOptions: {
        allowedFormats: ['pdf', 'excel', 'csv'],
        emailReports: true,
        cloudStorage: false,
        automaticBackup: true,
        shareWithAccountant: false
    }
};

/**
 * Default appearance settings
 */
const defaultAppearanceSettings: AppearanceSettings = {
    theme: 'system',
    colorScheme: {
        primary: '#3B82F6',
        secondary: '#64748B',
        accent: '#10B981',
        background: '#FFFFFF',
        surface: '#F8FAFC'
    },
    layout: {
        sidebarWidth: 'standard',
        compactMode: false,
        gridDensity: 'standard',
        showSidebar: true
    },
    display: {
        fontSize: 'medium',
        fontFamily: 'Inter',
        animations: true,
        reducedMotion: false,
        highContrast: false
    }
};

/**
 * Default integration settings
 */
const defaultIntegrationSettings: IntegrationSettings = {
    accounting: {
        provider: 'none',
        apiKey: '',
        syncFrequency: 'daily',
        syncSales: false,
        syncInventory: false,
        syncCustomers: false
    },
    ecommerce: {
        provider: 'none',
        storeUrl: '',
        apiCredentials: '',
        syncInventory: false,
        syncOrders: false,
        autoFulfillment: false
    },
    marketing: {
        emailProvider: 'none',
        smsProvider: 'none',
        socialMedia: {
            facebook: false,
            instagram: false,
            twitter: false,
            googleMyBusiness: false
        },
        analytics: {
            googleAnalytics: false,
            facebookPixel: false,
            customAnalytics: false
        }
    },
    paymentProcessors: {
        primaryProcessor: {
            provider: 'none',
            merchantId: '',
            apiKey: '',
            isActive: false,
            supportedMethods: []
        },
        backupProcessor: {
            provider: 'none',
            merchantId: '',
            apiKey: '',
            isActive: false,
            supportedMethods: []
        },
        fees: {
            creditCardRate: 2.9,
            debitCardRate: 1.9,
            transactionFee: 0.30,
            monthlyFee: 0
        },
        limits: {
            dailyLimit: 10000,
            transactionLimit: 2000,
            monthlyLimit: 100000,
            requireSignature: 25
        }
    }
};

/**
 * Default system settings
 */
const defaultSystemSettings: SystemSettings = {
    dataManagement: {
        automaticBackup: true,
        dataRetentionPeriod: 365,
        cloudSync: true,
        dataExportSchedule: 'weekly',
        archiveOldData: true,
        gdprCompliance: true
    },
    backup: {
        frequency: 'daily',
        retention: 30,
        cloudProvider: 'local',
        encryption: true,
        compression: true,
        verifyIntegrity: true
    },
    maintenance: {
        autoUpdates: false,
        maintenanceWindow: '02:00',
        updateChannel: 'stable',
        notifyBeforeUpdates: true,
        rollbackEnabled: true
    },
    performance: {
        cacheEnabled: true,
        imageCaching: true,
        databaseOptimization: true,
        logLevel: 'info',
        maxConcurrentUsers: 50
    }
};

/**
 * Settings store interface
 */
interface SettingsStore {
    // State
    settings: AppSettings;
    activities: SettingsActivity[];
    loading: {
        settings: boolean;
        saving: boolean;
    };
    errors: {
        settings: string | null;
        saving: string | null;
    };
    isDirty: boolean;

    // Settings Actions
    loadSettings: () => Promise<void>;
    updateSettings: (category: SettingsCategory, data: any) => Promise<void>;
    resetSettings: (category: SettingsCategory) => Promise<void>;
    exportSettings: () => Promise<string>;
    importSettings: (settingsJson: string) => Promise<void>;

    // Activity Actions
    logActivity: (
        category: SettingsCategory,
        action: 'created' | 'updated' | 'deleted' | 'reset' | 'imported' | 'exported',
        previousValue?: any,
        newValue?: any,
        details?: string
    ) => void;

    // Utility Actions
    getSettingsByCategory: (category: SettingsCategory) => any;
    getSetting: (category: SettingsCategory, key: string) => any;
    setSetting: (category: SettingsCategory, key: string, value: any) => void;
    resetAllSettings: () => Promise<void>;

    // Computed getters
    getRecentActivities: (limit?: number) => SettingsActivity[];
    hasUnsavedChanges: () => boolean;
}

/**
 * Generate activity ID
 */
const generateActivityId = (): string => {
    return `activity_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
};

/**
 * Create the settings store
 */
export const useSettingsStore = create<SettingsStore>()(
    devtools(
        persist(
            (set, get) => ({
                // Initial state
                settings: {
                    business: defaultBusinessSettings,
                    pos: defaultPOSSettings,
                    inventory: defaultInventorySettings,
                    payments: defaultPaymentSettings,
                    staff: defaultStaffSettings,
                    customers: defaultCustomerSettings,
                    notifications: defaultNotificationSettings,
                    reports: defaultReportSettings,
                    appearance: defaultAppearanceSettings,
                    integrations: defaultIntegrationSettings,
                    system: defaultSystemSettings
                },
                activities: [],
                loading: {
                    settings: false,
                    saving: false
                },
                errors: {
                    settings: null,
                    saving: null
                },
                isDirty: false,

                // Settings Actions
                loadSettings: async () => {
                    set(state => ({
                        loading: { ...state.loading, settings: true },
                        errors: { ...state.errors, settings: null }
                    }));

                    try {
                        // Simulate API call
                        await new Promise(resolve => setTimeout(resolve, 500));

                        set(state => ({
                            loading: { ...state.loading, settings: false }
                        }));
                    } catch (error) {
                        set(state => ({
                            loading: { ...state.loading, settings: false },
                            errors: {
                                ...state.errors,
                                settings: error instanceof Error ? error.message : 'Failed to load settings'
                            }
                        }));
                    }
                },

                updateSettings: async (category: SettingsCategory, data: any) => {
                    set(state => ({
                        loading: { ...state.loading, saving: true },
                        errors: { ...state.errors, saving: null }
                    }));

                    try {
                        // Validate settings data
                        const validation = validateSettingsData(category, data);
                        if (!validation.isValid) {
                            throw new Error('Invalid settings data');
                        }

                        const previousValue = get().settings[category];

                        // Simulate API call
                        await new Promise(resolve => setTimeout(resolve, 300));

                        set(state => ({
                            settings: {
                                ...state.settings,
                                [category]: { ...state.settings[category], ...data }
                            },
                            loading: { ...state.loading, saving: false },
                            isDirty: false
                        }));

                        // Log activity
                        get().logActivity(category, 'updated', previousValue, data);

                    } catch (error) {
                        set(state => ({
                            loading: { ...state.loading, saving: false },
                            errors: {
                                ...state.errors,
                                saving: error instanceof Error ? error.message : 'Failed to save settings'
                            }
                        }));
                        throw error;
                    }
                },

                resetSettings: async (category: SettingsCategory) => {
                    set(state => ({
                        loading: { ...state.loading, saving: true },
                        errors: { ...state.errors, saving: null }
                    }));

                    try {
                        const defaultData = getDefaultSettings(category);
                        const previousValue = get().settings[category];

                        // Simulate API call
                        await new Promise(resolve => setTimeout(resolve, 300));

                        set(state => ({
                            settings: {
                                ...state.settings,
                                [category]: defaultData
                            },
                            loading: { ...state.loading, saving: false },
                            isDirty: false
                        }));

                        // Log activity
                        get().logActivity(category, 'reset', previousValue, defaultData);

                    } catch (error) {
                        set(state => ({
                            loading: { ...state.loading, saving: false },
                            errors: {
                                ...state.errors,
                                saving: error instanceof Error ? error.message : 'Failed to reset settings'
                            }
                        }));
                        throw error;
                    }
                },

                exportSettings: async () => {
                    try {
                        const settings = get().settings;
                        const exportData = {
                            version: '1.0',
                            exportDate: new Date().toISOString(),
                            settings,
                            metadata: {
                                appVersion: '1.0.0',
                                storeName: settings.business.storeName,
                                totalCategories: Object.keys(settings).length
                            }
                        };

                        // Log activity
                        get().logActivity('system', 'exported', null, null, 'Settings exported to file');

                        return JSON.stringify(exportData, null, 2);
                    } catch (error) {
                        throw new Error('Failed to export settings');
                    }
                },

                importSettings: async (settingsJson: string) => {
                    set(state => ({
                        loading: { ...state.loading, saving: true },
                        errors: { ...state.errors, saving: null }
                    }));

                    try {
                        const importData = JSON.parse(settingsJson);

                        if (!importData.settings) {
                            throw new Error('Invalid settings file format');
                        }

                        // Simulate API call
                        await new Promise(resolve => setTimeout(resolve, 500));

                        set(state => ({
                            settings: { ...state.settings, ...importData.settings },
                            loading: { ...state.loading, saving: false },
                            isDirty: false
                        }));

                        // Log activity
                        get().logActivity('system', 'imported', null, importData.settings);

                    } catch (error) {
                        set(state => ({
                            loading: { ...state.loading, saving: false },
                            errors: {
                                ...state.errors,
                                saving: error instanceof Error ? error.message : 'Failed to import settings'
                            }
                        }));
                        throw error;
                    }
                },

                // Activity Actions
                logActivity: (
                    category: SettingsCategory,
                    action: 'created' | 'updated' | 'deleted' | 'reset' | 'imported' | 'exported',
                    previousValue?: any,
                    newValue?: any,
                    details?: string
                ) => {
                    const activity: SettingsActivity = {
                        id: generateActivityId(),
                        category,
                        action,
                        previousValue,
                        newValue,
                        timestamp: new Date(),
                        details
                    };

                    set(state => ({
                        activities: [activity, ...state.activities.slice(0, 99)] // Keep last 100 activities
                    }));
                },

                // Utility Actions
                getSettingsByCategory: (category: SettingsCategory) => {
                    return get().settings[category] || {};
                },

                getSetting: (category: SettingsCategory, key: string) => {
                    const categorySettings = get().settings[category];
                    return categorySettings?.[key as keyof typeof categorySettings];
                },

                setSetting: (category: SettingsCategory, key: string, value: any) => {
                    set(state => ({
                        settings: {
                            ...state.settings,
                            [category]: {
                                ...state.settings[category],
                                [key]: value
                            }
                        },
                        isDirty: true
                    }));
                },

                resetAllSettings: async () => {
                    set(state => ({
                        loading: { ...state.loading, saving: true },
                        errors: { ...state.errors, saving: null }
                    }));

                    try {
                        // Simulate API call
                        await new Promise(resolve => setTimeout(resolve, 1000));

                        set(state => ({
                            settings: {
                                business: defaultBusinessSettings,
                                pos: defaultPOSSettings,
                                inventory: defaultInventorySettings,
                                payments: defaultPaymentSettings,
                                staff: defaultStaffSettings,
                                customers: defaultCustomerSettings,
                                notifications: defaultNotificationSettings,
                                reports: defaultReportSettings,
                                appearance: defaultAppearanceSettings,
                                integrations: defaultIntegrationSettings,
                                system: defaultSystemSettings
                            },
                            loading: { ...state.loading, saving: false },
                            isDirty: false
                        }));

                        // Log activity
                        get().logActivity('system', 'reset', null, null, 'All settings reset to defaults');

                    } catch (error) {
                        set(state => ({
                            loading: { ...state.loading, saving: false },
                            errors: {
                                ...state.errors,
                                saving: error instanceof Error ? error.message : 'Failed to reset all settings'
                            }
                        }));
                        throw error;
                    }
                },

                // Computed getters
                getRecentActivities: (limit: number = 10) => {
                    return get().activities.slice(0, limit);
                },

                hasUnsavedChanges: () => {
                    return get().isDirty;
                }
            }),
            {
                name: 'settings-store',
                partialize: (state) => ({
                    settings: state.settings,
                    activities: state.activities.slice(0, 50) // Persist only last 50 activities
                })
            }
        ),
        {
            name: 'settings-store'
        }
    )
); 