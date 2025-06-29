import type {
    SettingsCategory,
    GeneralSettings,
    AppearanceSettings,
    NotificationSettings,
    SecuritySettings,
    SettingsActivity,
    SettingsAction
} from '../../types/settings';

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
export function getDefaultSettings(category: string): any {
    const defaults = {
        general: {
            appName: 'RetailUp',
            companyName: 'My Store',
            email: 'info@mystore.com',
            phone: '+1 (555) 123-4567',
            timezone: 'America/New_York',
            currency: 'USD'
        },
        appearance: {
            theme: 'system',
            primaryColor: '#3B82F6',
            fontSize: 'medium',
            compactMode: false
        },
        notifications: {
            emailNotifications: true,
            pushNotifications: true,
            salesAlerts: true,
            inventoryAlerts: true
        },
        security: {
            twoFactorAuth: false,
            sessionTimeout: 60
        }
    };

    return defaults[category as keyof typeof defaults] || {};
}

/**
 * Validates settings data for a specific category
 * @param category - Settings category
 * @param data - Settings data to validate
 * @returns Validation result
 */
export function validateSettingsData(category: string, data: any): { isValid: boolean; errors?: string[] } {
    if (!data || typeof data !== 'object') {
        return { isValid: false, errors: ['Invalid settings data'] };
    }
    return { isValid: true };
}

/**
 * Formats settings category name for display
 * @param category - Settings category
 * @returns Formatted category name
 */
export function formatCategoryName(category: SettingsCategory): string {
    const nameMap: Record<SettingsCategory, string> = {
        general: 'General',
        appearance: 'Appearance',
        notifications: 'Notifications',
        security: 'Security',
        payment: 'Payment',
        inventory: 'Inventory',
        pos: 'Point of Sale',
        reporting: 'Reporting',
        integrations: 'Integrations',
        backup: 'Backup'
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
export function sanitizeSettingsForExport(settings: Record<SettingsCategory, any>): Record<SettingsCategory, any> {
    const sanitized = { ...settings };

    // Remove sensitive data before export
    if (sanitized.security) {
        delete sanitized.security.twoFactorAuth?.backupCodes;
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
        general: 'Settings',
        appearance: 'Palette',
        notifications: 'Bell',
        security: 'Shield',
        payment: 'CreditCard',
        inventory: 'Package',
        pos: 'ShoppingCart',
        reporting: 'BarChart3',
        integrations: 'Plug',
        backup: 'HardDrive'
    };

    return iconMap[category] || 'Settings';
}

/**
 * Gets settings color by category
 * @param category - Settings category
 * @returns Color class
 */
export function getCategoryColor(category: SettingsCategory): string {
    const colorMap: Record<SettingsCategory, string> = {
        general: 'bg-blue-500',
        appearance: 'bg-purple-500',
        notifications: 'bg-orange-500',
        security: 'bg-red-500',
        payment: 'bg-green-500',
        inventory: 'bg-amber-500',
        pos: 'bg-teal-500',
        reporting: 'bg-indigo-500',
        integrations: 'bg-pink-500',
        backup: 'bg-gray-500'
    };

    return colorMap[category] || 'bg-gray-500';
} 