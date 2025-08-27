package com.sajjadkademm.retail.config.locales;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.AcceptHeaderLocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

@Configuration
public class LocaleConfig {

    /**
     * Configure the locale resolver with session-based storage and Accept-Language
     * header fallback
     */
    @Bean
    public LocaleResolver localeResolver() {
        SessionLocaleResolver resolver = new SessionLocaleResolver();
        resolver.setDefaultLocale(Locale.ENGLISH);
        return resolver;
    }

    /**
     * Configure the message source for internationalization with proper fallbacks
     */
    @Bean
    public ResourceBundleMessageSource messageSource() {
        ResourceBundleMessageSource source = new ResourceBundleMessageSource();
        source.setBasenames("messages"); // Base name for message properties files
        source.setDefaultEncoding("UTF-8"); // Support Arabic characters
        source.setUseCodeAsDefaultMessage(true); // Use code as fallback if message not found
        source.setFallbackToSystemLocale(false); // Don't fall back to system locale
        source.setCacheSeconds(3600); // Cache messages for 1 hour
        return source;
    }

    /**
     * Configure locale change interceptor for dynamic locale switching
     * Allows changing locale via URL parameter (e.g., ?lang=ar)
     */
    @Bean
    public LocaleChangeInterceptor localeChangeInterceptor() {
        LocaleChangeInterceptor interceptor = new LocaleChangeInterceptor();
        interceptor.setParamName("lang");
        return interceptor;
    }

    /**
     * Get list of supported locales
     */
    public static List<Locale> getSupportedLocales() {
        return Arrays.asList(
                Locale.ENGLISH,
                new Locale("ar", "SA") // Arabic (Saudi Arabia)
        );
    }

    /**
     * Check if a locale is supported
     */
    public static boolean isSupportedLocale(Locale locale) {
        return getSupportedLocales().stream()
                .anyMatch(supported -> supported.getLanguage().equals(locale.getLanguage()));
    }

    /**
     * Get default locale
     */
    public static Locale getDefaultLocale() {
        return Locale.ENGLISH;
    }
}
