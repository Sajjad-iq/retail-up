import { z } from 'zod';

/**
 * Helper function to validate time format (HH:MM)
 */
const isValidTime = (time: string): boolean => {
    const timeRegex = /^([01]?[0-9]|2[0-3]):[0-5][0-9]$/;
    return timeRegex.test(time);
};

/**
 * Helper function to validate phone numbers
 */
const isValidPhone = (phone: string): boolean => {
    const phoneRegex = /^\+?[\d\s\-\(\)]{10,}$/;
    return phoneRegex.test(phone);
};

/**
 * Helper function to validate email addresses
 */
const isValidEmail = (email: string): boolean => {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(email);
};

/**
 * Helper function to validate hex color codes
 */
const isValidHexColor = (color: string): boolean => {
    const hexRegex = /^#[0-9A-Fa-f]{6}$/;
    return hexRegex.test(color);
};

/**
 * Store address validation schema
 */
export const storeAddressSchema = z.object({
    street: z.string()
        .min(1, 'Street address is required')
        .max(100, 'Street address must not exceed 100 characters'),
    city: z.string()
        .min(1, 'City is required')
        .max(50, 'City must not exceed 50 characters'),
    state: z.string()
        .min(1, 'State is required')
        .max(50, 'State must not exceed 50 characters'),
    postalCode: z.string()
        .min(1, 'Postal code is required')
        .max(20, 'Postal code must not exceed 20 characters'),
    country: z.string()
        .min(1, 'Country is required')
        .max(50, 'Country must not exceed 50 characters'),
});

/**
 * Contact information validation schema
 */
export const contactInfoSchema = z.object({
    phone: z.string()
        .min(1, 'Phone number is required')
        .refine(isValidPhone, {
            message: 'Please enter a valid phone number'
        }),
    email: z.string()
        .min(1, 'Email is required')
        .refine(isValidEmail, {
            message: 'Please enter a valid email address'
        }),
    website: z.string()
        .url('Please enter a valid website URL')
        .optional(),
    socialMedia: z.object({
        facebook: z.string().url('Please enter a valid Facebook URL').optional(),
        twitter: z.string().url('Please enter a valid Twitter URL').optional(),
        instagram: z.string().url('Please enter a valid Instagram URL').optional(),
        linkedin: z.string().url('Please enter a valid LinkedIn URL').optional(),
    }).optional(),
});

/**
 * Day hours validation schema
 */
export const dayHoursSchema = z.object({
    isOpen: z.boolean(),
    openTime: z.string()
        .optional()
        .refine((time) => !time || isValidTime(time), {
            message: 'Please enter a valid time (HH:MM)'
        }),
    closeTime: z.string()
        .optional()
        .refine((time) => !time || isValidTime(time), {
            message: 'Please enter a valid time (HH:MM)'
        }),
    breaks: z.array(z.object({
        startTime: z.string()
            .refine(isValidTime, {
                message: 'Please enter a valid time (HH:MM)'
            }),
        endTime: z.string()
            .refine(isValidTime, {
                message: 'Please enter a valid time (HH:MM)'
            }),
        description: z.string().max(100, 'Description must not exceed 100 characters').optional(),
    })).optional(),
}).refine((data) => {
    if (data.isOpen && (!data.openTime || !data.closeTime)) {
        return false;
    }
    return true;
}, {
    message: 'Open and close times are required when store is open',
    path: ['openTime'],
});

/**
 * Business hours validation schema
 */
export const businessHoursSchema = z.object({
    monday: dayHoursSchema,
    tuesday: dayHoursSchema,
    wednesday: dayHoursSchema,
    thursday: dayHoursSchema,
    friday: dayHoursSchema,
    saturday: dayHoursSchema,
    sunday: dayHoursSchema,
    holidays: z.array(z.object({
        name: z.string().min(1, 'Holiday name is required'),
        date: z.date({
            required_error: 'Holiday date is required',
            invalid_type_error: 'Please enter a valid date',
        }),
        hours: dayHoursSchema,
    })).optional(),
});

/**
 * Currency validation schema
 */
export const currencySchema = z.object({
    code: z.string()
        .length(3, 'Currency code must be 3 characters')
        .regex(/^[A-Z]{3}$/, 'Currency code must be uppercase letters'),
    symbol: z.string()
        .min(1, 'Currency symbol is required')
        .max(5, 'Currency symbol must not exceed 5 characters'),
    name: z.string()
        .min(1, 'Currency name is required')
        .max(50, 'Currency name must not exceed 50 characters'),
    decimalPlaces: z.number()
        .min(0, 'Decimal places cannot be negative')
        .max(4, 'Decimal places cannot exceed 4'),
});

/**
 * Tax category validation schema
 */
export const taxCategorySchema = z.object({
    id: z.string().min(1, 'Tax category ID is required'),
    name: z.string()
        .min(1, 'Tax category name is required')
        .max(50, 'Name must not exceed 50 characters'),
    rate: z.number()
        .min(0, 'Tax rate cannot be negative')
        .max(100, 'Tax rate cannot exceed 100%'),
    description: z.string()
        .max(200, 'Description must not exceed 200 characters')
        .optional(),
});

/**
 * General settings validation schema
 */
export const generalSettingsSchema = z.object({
    appName: z.string()
        .min(1, 'App name is required')
        .max(50, 'App name must not exceed 50 characters'),
    companyName: z.string()
        .min(1, 'Company name is required')
        .max(100, 'Company name must not exceed 100 characters'),
    storeAddress: storeAddressSchema,
    contactInfo: contactInfoSchema,
    businessHours: businessHoursSchema,
    currency: currencySchema,
    timezone: z.string().min(1, 'Timezone is required'),
    language: z.string().min(1, 'Language is required'),
    taxSettings: z.object({
        defaultTaxRate: z.number()
            .min(0, 'Tax rate cannot be negative')
            .max(100, 'Tax rate cannot exceed 100%'),
        taxInclusive: z.boolean(),
        taxCategories: z.array(taxCategorySchema),
        taxExemptions: z.array(z.object({
            id: z.string().min(1, 'Exemption ID is required'),
            name: z.string().min(1, 'Exemption name is required'),
            type: z.enum(['product', 'customer', 'transaction']),
            criteria: z.string().min(1, 'Exemption criteria is required'),
        })),
    }),
});

/**
 * Appearance settings validation schema
 */
export const appearanceSettingsSchema = z.object({
    theme: z.enum(['light', 'dark', 'system']),
    primaryColor: z.string()
        .refine(isValidHexColor, {
            message: 'Please enter a valid hex color code'
        }),
    accentColor: z.string()
        .refine(isValidHexColor, {
            message: 'Please enter a valid hex color code'
        }),
    fontFamily: z.string().min(1, 'Font family is required'),
    fontSize: z.enum(['small', 'medium', 'large']),
    sidebarPosition: z.enum(['left', 'right']),
    compactMode: z.boolean(),
    showAnimations: z.boolean(),
    customCss: z.string()
        .max(10000, 'Custom CSS must not exceed 10,000 characters')
        .optional(),
});

/**
 * Notification settings validation schema
 */
export const notificationSettingsSchema = z.object({
    emailNotifications: z.object({
        enabled: z.boolean(),
        sales: z.boolean(),
        inventory: z.boolean(),
        system: z.boolean(),
        customer: z.boolean(),
        emailAddress: z.string()
            .refine((email) => !email || isValidEmail(email), {
                message: 'Please enter a valid email address'
            })
            .optional(),
    }),
    pushNotifications: z.object({
        enabled: z.boolean(),
        sales: z.boolean(),
        inventory: z.boolean(),
        system: z.boolean(),
        customer: z.boolean(),
    }),
    smsNotifications: z.object({
        enabled: z.boolean(),
        criticalOnly: z.boolean(),
        phoneNumber: z.string()
            .refine((phone) => !phone || isValidPhone(phone), {
                message: 'Please enter a valid phone number'
            })
            .optional(),
    }),
    inAppNotifications: z.object({
        enabled: z.boolean(),
        desktop: z.boolean(),
        sound: z.boolean(),
        badges: z.boolean(),
    }),
    frequency: z.enum(['immediate', 'hourly', 'daily', 'weekly']),
    quietHours: z.object({
        enabled: z.boolean(),
        startTime: z.string()
            .refine(isValidTime, {
                message: 'Please enter a valid time (HH:MM)'
            }),
        endTime: z.string()
            .refine(isValidTime, {
                message: 'Please enter a valid time (HH:MM)'
            }),
        daysOfWeek: z.array(z.string()),
    }),
});

/**
 * Security settings validation schema
 */
export const securitySettingsSchema = z.object({
    twoFactorAuth: z.object({
        enabled: z.boolean(),
        method: z.enum(['sms', 'email', 'authenticator']),
        backupCodes: z.array(z.string()).optional(),
    }),
    sessionSettings: z.object({
        timeout: z.number()
            .min(5, 'Session timeout must be at least 5 minutes')
            .max(1440, 'Session timeout cannot exceed 24 hours'),
        rememberMeDuration: z.number()
            .min(1, 'Remember me duration must be at least 1 day')
            .max(365, 'Remember me duration cannot exceed 365 days'),
        concurrentSessions: z.number()
            .min(1, 'Must allow at least 1 concurrent session')
            .max(10, 'Cannot exceed 10 concurrent sessions'),
    }),
    passwordPolicy: z.object({
        minLength: z.number()
            .min(6, 'Minimum password length must be at least 6')
            .max(128, 'Maximum password length cannot exceed 128'),
        requireUppercase: z.boolean(),
        requireLowercase: z.boolean(),
        requireNumbers: z.boolean(),
        requireSpecialChars: z.boolean(),
        expiryDays: z.number()
            .min(30, 'Password expiry must be at least 30 days')
            .max(365, 'Password expiry cannot exceed 365 days')
            .optional(),
        historyCount: z.number()
            .min(0, 'Password history count cannot be negative')
            .max(24, 'Password history count cannot exceed 24'),
    }),
    loginAttempts: z.object({
        maxAttempts: z.number()
            .min(3, 'Must allow at least 3 login attempts')
            .max(10, 'Cannot exceed 10 login attempts'),
        lockoutDuration: z.number()
            .min(5, 'Lockout duration must be at least 5 minutes')
            .max(1440, 'Lockout duration cannot exceed 24 hours'),
        resetAfter: z.number()
            .min(10, 'Reset time must be at least 10 minutes')
            .max(1440, 'Reset time cannot exceed 24 hours'),
    }),
    dataEncryption: z.object({
        encryptionAtRest: z.boolean(),
        encryptionInTransit: z.boolean(),
        algorithm: z.string().min(1, 'Encryption algorithm is required'),
    }),
    auditLogging: z.object({
        enabled: z.boolean(),
        logLevel: z.enum(['error', 'warn', 'info', 'debug']),
        retentionDays: z.number()
            .min(30, 'Log retention must be at least 30 days')
            .max(2555, 'Log retention cannot exceed 7 years'),
    }),
});


/**
 * Settings form validation schema
 */
export const settingsFormSchema = z.object({
    category: z.enum(['general', 'appearance', 'notifications', 'security', 'payment', 'inventory', 'pos', 'reporting', 'integrations', 'backup']),
    data: z.any(), // This will be validated based on the category
});

// Type exports
export type GeneralSettingsInput = z.infer<typeof generalSettingsSchema>;
export type AppearanceSettingsInput = z.infer<typeof appearanceSettingsSchema>;
export type NotificationSettingsInput = z.infer<typeof notificationSettingsSchema>;
export type SecuritySettingsInput = z.infer<typeof securitySettingsSchema>;
export type SettingsFormInput = z.infer<typeof settingsFormSchema>;
export type StoreAddressInput = z.infer<typeof storeAddressSchema>;
export type ContactInfoInput = z.infer<typeof contactInfoSchema>;
export type BusinessHoursInput = z.infer<typeof businessHoursSchema>;
export type CurrencyInput = z.infer<typeof currencySchema>;

// Legacy function exports for backward compatibility
export const validateGeneralSettings = (data: any) => {
    const result = generalSettingsSchema.safeParse(data);

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

export const validateAppearanceSettings = (data: any) => {
    const result = appearanceSettingsSchema.safeParse(data);

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

export const validateNotificationSettings = (data: any) => {
    const result = notificationSettingsSchema.safeParse(data);

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

export const validateSecuritySettings = (data: any) => {
    const result = securitySettingsSchema.safeParse(data);

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