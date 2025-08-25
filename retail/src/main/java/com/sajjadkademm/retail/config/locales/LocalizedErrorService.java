package com.sajjadkademm.retail.config.locales;

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
        Locale currentLocale = LocaleContextHolder.getLocale();
        return messageSource.getMessage(messageCode, null, currentLocale);
    }

    /**
     * Get localized error message for the given message code with arguments
     * Uses the current locale from LocaleContextHolder
     */
    public String getLocalizedMessage(String messageCode, Object... args) {
        Locale currentLocale = LocaleContextHolder.getLocale();
        return messageSource.getMessage(messageCode, args, currentLocale);
    }

    /**
     * Get localized error message for the given message code with specific locale
     */
    public String getLocalizedMessage(String messageCode, Locale locale) {
        return messageSource.getMessage(messageCode, null, locale);
    }

    /**
     * Get localized error message for the given message code with arguments and
     * specific locale
     */
    public String getLocalizedMessage(String messageCode, Locale locale, Object... args) {
        return messageSource.getMessage(messageCode, args, locale);
    }

    /**
     * Get current locale from context
     */
    public Locale getCurrentLocale() {
        return LocaleContextHolder.getLocale();
    }

    /**
     * Check if current locale is Arabic
     */
    public boolean isArabicLocale() {
        Locale currentLocale = LocaleContextHolder.getLocale();
        return "ar".equals(currentLocale.getLanguage());
    }
}
