package com.sajjadkademm.retail.shared.localization;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

import java.util.Locale;

/**
 * Service for handling localized error messages
 * Provides methods to get error messages in the current locale
 */
@Service
public class LocalizedErrorService {

    private final MessageSource messageSource;

    @Autowired
    public LocalizedErrorService(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    /**
     * Get localized error message for the given message code
     * Uses the current locale from LocaleContextHolder
     */
    public String getLocalizedMessage(String messageCode) {
        Locale currentLocale = getCurrentLocale();
        return messageSource.getMessage(messageCode, null, messageCode, currentLocale);
    }

    /**
     * Get localized error message for the given message code with arguments
     * Uses the current locale from LocaleContextHolder
     */
    public String getLocalizedMessage(String messageCode, Object... args) {
        Locale currentLocale = getCurrentLocale();
        return messageSource.getMessage(messageCode, args, messageCode, currentLocale);
    }

    /**
     * Get localized error message for the given message code with specific locale
     */
    public String getLocalizedMessage(String messageCode, Locale locale) {
        return messageSource.getMessage(messageCode, null, messageCode, locale);
    }

    /**
     * Get localized error message for the given message code with arguments and
     * specific locale
     */
    public String getLocalizedMessage(String messageCode, Locale locale, Object... args) {
        return messageSource.getMessage(messageCode, args, messageCode, locale);
    }

    /**
     * Get current locale from context with fallback to default
     */
    public Locale getCurrentLocale() {
        Locale currentLocale = LocaleContextHolder.getLocale();

        // If no locale is set or it's the default locale, return English
        if (currentLocale == null || Locale.getDefault().equals(currentLocale)) {
            return LocaleConfig.getDefaultLocale();
        }

        // Validate that the current locale is supported
        if (!LocaleConfig.isSupportedLocale(currentLocale)) {
            return LocaleConfig.getDefaultLocale();
        }

        return currentLocale;
    }

    /**
     * Check if current locale is Arabic
     */
    public boolean isArabicLocale() {
        Locale currentLocale = getCurrentLocale();
        return "ar".equals(currentLocale.getLanguage());
    }

    /**
     * Check if current locale is English
     */
    public boolean isEnglishLocale() {
        Locale currentLocale = getCurrentLocale();
        return "en".equals(currentLocale.getLanguage());
    }

    /**
     * Get locale display name in the current locale
     */
    public String getLocaleDisplayName() {
        Locale currentLocale = getCurrentLocale();
        return currentLocale.getDisplayLanguage(currentLocale);
    }

    /**
     * Get supported locales list
     */
    public java.util.List<Locale> getSupportedLocales() {
        return LocaleConfig.getSupportedLocales();
    }

    /**
     * Validate if a locale is supported
     */
    public boolean isLocaleSupported(Locale locale) {
        return LocaleConfig.isSupportedLocale(locale);
    }

    /**
     * Get fallback locale if current locale is not supported
     */
    public Locale getFallbackLocale() {
        return LocaleConfig.getDefaultLocale();
    }
}
