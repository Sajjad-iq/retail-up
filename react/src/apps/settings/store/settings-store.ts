import { create } from 'zustand';
import { devtools, persist } from 'zustand/middleware';
import type {
    AppSettings,
    SettingsCategory,
    SettingsData,
    GeneralSettings,
    AppearanceSettings,
    NotificationSettings,
    SecuritySettings,
    SettingsActivity,
    SettingsAction
} from '../types/settings';
import {
    generateSettingsId,
    generateActivityId,
    getDefaultSettings,
    validateSettingsData
} from '../lib/utils/settings-utils';

/**
 * Default general settings
 */
const defaultGeneralSettings: GeneralSettings = {
    appName: 'RetailUp',
    companyName: 'My Store',
    storeAddress: {
        street: '123 Main St',
        city: 'Anytown',
        state: 'State',
        postalCode: '12345',
        country: 'United States'
    },
    contactInfo: {
        phone: '+1 (555) 123-4567',
        email: 'info@mystore.com',
        website: 'https://mystore.com',
        socialMedia: {
            facebook: '',
            twitter: '',
            instagram: '',
            linkedin: ''
        }
    },
    businessHours: {
        monday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        tuesday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        wednesday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        thursday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        friday: { isOpen: true, openTime: '09:00', closeTime: '17:00' },
        saturday: { isOpen: true, openTime: '10:00', closeTime: '16:00' },
        sunday: { isOpen: false }
    },
    currency: {
        code: 'USD',
        symbol: '$',
        name: 'US Dollar',
        decimalPlaces: 2
    },
    timezone: 'America/New_York',
    language: 'en',
    taxSettings: {
        defaultTaxRate: 8.25,
        taxInclusive: false,
        taxCategories: [],
        taxExemptions: []
    }
};

/**
 * Default appearance settings
 */
const defaultAppearanceSettings: AppearanceSettings = {
    theme: 'system',
    primaryColor: '#3B82F6',
    accentColor: '#10B981',
    fontFamily: 'Inter',
    fontSize: 'medium',
    sidebarPosition: 'left',
    compactMode: false,
    showAnimations: true
};

/**
 * Default notification settings
 */
const defaultNotificationSettings: NotificationSettings = {
    emailNotifications: {
        enabled: true,
        sales: true,
        inventory: true,
        system: true,
        customer: false
    },
    pushNotifications: {
        enabled: true,
        sales: true,
        inventory: true,
        system: true,
        customer: false
    },
    smsNotifications: {
        enabled: false,
        criticalOnly: true
    },
    inAppNotifications: {
        enabled: true,
        desktop: true,
        sound: true,
        badges: true
    },
    frequency: 'immediate',
    quietHours: {
        enabled: false,
        startTime: '22:00',
        endTime: '08:00',
        daysOfWeek: ['saturday', 'sunday']
    }
};

/**
 * Default security settings
 */
const defaultSecuritySettings: SecuritySettings = {
    twoFactorAuth: {
        enabled: false,
        method: 'email'
    },
    sessionSettings: {
        timeout: 60,
        rememberMeDuration: 30,
        concurrentSessions: 3
    },
    passwordPolicy: {
        minLength: 8,
        requireUppercase: true,
        requireLowercase: true,
        requireNumbers: true,
        requireSpecialChars: false,
        historyCount: 5
    },
    loginAttempts: {
        maxAttempts: 5,
        lockoutDuration: 30,
        resetAfter: 60
    },
    dataEncryption: {
        encryptionAtRest: true,
        encryptionInTransit: true,
        algorithm: 'AES-256'
    },
    auditLogging: {
        enabled: true,
        logLevel: 'info',
        retentionDays: 90
    }
};

/**
 * Settings store interface
 */
interface SettingsStore {
    // State
    settings: Record<SettingsCategory, any>;
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
        action: SettingsAction,
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
 * Create the settings store
 */
export const useSettingsStore = create<SettingsStore>()(
    devtools(
        persist(
            (set, get) => ({
                // Initial state
                settings: {
                    general: defaultGeneralSettings,
                    appearance: defaultAppearanceSettings,
                    notifications: defaultNotificationSettings,
                    security: defaultSecuritySettings,
                    payment: {},
                    inventory: {},
                    pos: {},
                    reporting: {},
                    integrations: {},
                    backup: {}
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
                            timestamp: new Date().toISOString(),
                            settings
                        };

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
                        get().logActivity('general', 'imported', null, importData.settings);

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
                    action: SettingsAction,
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
                    return categorySettings?.[key];
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
                                general: defaultGeneralSettings,
                                appearance: defaultAppearanceSettings,
                                notifications: defaultNotificationSettings,
                                security: defaultSecuritySettings,
                                payment: {},
                                inventory: {},
                                pos: {},
                                reporting: {},
                                integrations: {},
                                backup: {}
                            },
                            loading: { ...state.loading, saving: false },
                            isDirty: false
                        }));

                        // Log activity
                        get().logActivity('general', 'reset', null, null, 'All settings reset to defaults');

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