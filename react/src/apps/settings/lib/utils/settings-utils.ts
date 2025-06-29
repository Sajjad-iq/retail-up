import type {
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
    SettingsValidationResult
} from '../../types/settings';

/**
 * Settings activity action types
 */
export type SettingsAction = 'created' | 'updated' | 'deleted' | 'reset' | 'imported' | 'exported';

/**
 * Settings activity log entry
 */
export interface SettingsActivity {
    id: string;
    category: SettingsCategory;
    action: SettingsAction;
    previousValue?: any;
    newValue?: any;
    timestamp: Date;
    details?: string;
}

/**
 * Generates a unique settings ID
 * @returns Unique settings identifier
 */
export function generateSettingsId(): string {
    return `settings-${Date.now()}-${Math.random().toString(36).substring(2, 8)}`;
}

/**
 * Generates a unique activity ID
 * @returns Unique activity identifier
 */
export function generateActivityId(): string {
    return `activity-${Date.now()}-${Math.random().toString(36).substring(2, 8)}`;
}

/**
 * Gets default settings for a specific category
 * @param category - Settings category
 * @returns Default settings for the category
 */
export function getDefaultSettings(category: SettingsCategory): any {
    const defaults: Record<SettingsCategory, any> = {
        business: {
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
        },
        pos: {
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
        },
        inventory: {
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
        },
        payments: {
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
        },
        staff: {
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
        },
        customers: {
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
        },
        notifications: {
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
        },
        reports: {
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
        },
        appearance: {
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
        },
        integrations: {
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
        },
        system: {
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
        }
    };

    return defaults[category] || {};
}

/**
 * Validates settings data for a specific category
 * @param category - Settings category
 * @param data - Settings data to validate
 * @returns Validation result
 */
export function validateSettingsData(category: SettingsCategory, data: any): SettingsValidationResult {
    if (!data || typeof data !== 'object') {
        return {
            isValid: false,
            errors: [{ field: 'root', message: 'Invalid settings data', code: 'INVALID_DATA' }],
            warnings: []
        };
    }
    return { isValid: true, errors: [], warnings: [] };
}

/**
 * Formats settings category name for display
 * @param category - Settings category
 * @returns Formatted category name
 */
export function formatCategoryName(category: SettingsCategory): string {
    const nameMap: Record<SettingsCategory, string> = {
        business: 'Business & Store',
        pos: 'Point of Sale',
        inventory: 'Inventory',
        payments: 'Payments & Tax',
        staff: 'Staff & Security',
        customers: 'Customers',
        notifications: 'Notifications',
        reports: 'Reports & Analytics',
        appearance: 'Appearance',
        integrations: 'Integrations',
        system: 'System & Backup'
    };

    return nameMap[category] || category;
}

/**
 * Formats activity action for display
 * @param action - Settings action
 * @returns Formatted action text
 */
export function formatActionName(action: SettingsAction): string {
    const actionMap: Record<SettingsAction, string> = {
        created: 'Created',
        updated: 'Updated',
        deleted: 'Deleted',
        reset: 'Reset',
        imported: 'Imported',
        exported: 'Exported'
    };

    return actionMap[action] || action;
}

/**
 * Formats date for display
 * @param date - Date to format
 * @returns Formatted date string
 */
export function formatDate(date: Date): string {
    return date.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit'
    });
}

/**
 * Deep merge two objects
 * @param target - Target object
 * @param source - Source object
 * @returns Merged object
 */
export function deepMerge(target: any, source: any): any {
    if (source === null || typeof source !== 'object') {
        return source;
    }

    if (target === null || typeof target !== 'object') {
        return source;
    }

    const result = { ...target };

    for (const key in source) {
        if (source.hasOwnProperty(key)) {
            if (typeof source[key] === 'object' && source[key] !== null && !Array.isArray(source[key])) {
                result[key] = deepMerge(target[key], source[key]);
            } else {
                result[key] = source[key];
            }
        }
    }

    return result;
}

/**
 * Checks if settings have changed
 * @param original - Original settings
 * @param current - Current settings
 * @returns Whether settings have changed
 */
export function hasSettingsChanged(original: any, current: any): boolean {
    return JSON.stringify(original) !== JSON.stringify(current);
}

/**
 * Sanitizes settings data for export
 * @param settings - Settings data
 * @returns Sanitized settings data
 */
export function sanitizeSettingsForExport(settings: any): any {
    const sanitized = { ...settings };

    // Remove sensitive data before export
    if (sanitized.staff?.security) {
        // Remove sensitive security data
        if (sanitized.staff.security.passwordPolicy) {
            delete sanitized.staff.security.passwordPolicy;
        }
    }

    return sanitized;
}

/**
 * Gets settings icon by category
 * @param category - Settings category
 * @returns Icon name
 */
export function getCategoryIcon(category: SettingsCategory): string {
    const iconMap: Record<SettingsCategory, string> = {
        business: 'Store',
        pos: 'ShoppingCart',
        inventory: 'Package',
        payments: 'CreditCard',
        staff: 'Users',
        customers: 'UserCheck',
        notifications: 'Bell',
        reports: 'BarChart3',
        appearance: 'Palette',
        integrations: 'Plug',
        system: 'HardDrive'
    };

    return iconMap[category] || 'Settings';
}

/**
 * Gets settings category color
 * @param category - Settings category
 * @returns Color class
 */
export function getCategoryColor(category: SettingsCategory): string {
    const colorMap: Record<SettingsCategory, string> = {
        business: 'bg-blue-500',
        pos: 'bg-green-500',
        inventory: 'bg-amber-500',
        payments: 'bg-emerald-500',
        staff: 'bg-purple-500',
        customers: 'bg-pink-500',
        notifications: 'bg-orange-500',
        reports: 'bg-indigo-500',
        appearance: 'bg-violet-500',
        integrations: 'bg-cyan-500',
        system: 'bg-gray-500'
    };

    return colorMap[category] || 'bg-gray-500';
} 