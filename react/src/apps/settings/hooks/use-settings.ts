import { useSettingsStore } from '../store/settings-store';

/**
 * Custom hook for settings operations
 * Provides settings state and management functionality
 */
export function useSettings() {
    const {
        settings,
        activities,
        loading,
        errors,
        isDirty,
        loadSettings,
        updateSettings,
        resetSettings,
        exportSettings,
        importSettings,
        logActivity,
        getSettingsByCategory,
        getSetting,
        setSetting,
        resetAllSettings,
        getRecentActivities,
        hasUnsavedChanges
    } = useSettingsStore();

    return {
        // State
        settings,
        activities,
        loading,
        errors,
        isDirty,

        // Actions
        loadSettings,
        updateSettings,
        resetSettings,
        exportSettings,
        importSettings,
        logActivity,
        getSettingsByCategory,
        getSetting,
        setSetting,
        resetAllSettings,

        // Computed
        getRecentActivities,
        hasUnsavedChanges
    };
}

/**
 * Hook for specific settings category
 */
export function useCategorySettings(category: string) {
    const { getSettingsByCategory, setSetting, updateSettings } = useSettings();

    const categorySettings = getSettingsByCategory(category as any);

    const updateCategorySetting = (key: string, value: any) => {
        setSetting(category as any, key, value);
    };

    const saveCategorySettings = async (data: any) => {
        return updateSettings(category as any, data);
    };

    return {
        settings: categorySettings,
        updateSetting: updateCategorySetting,
        saveSettings: saveCategorySettings
    };
} 