package com.sajjadkademm.retail.shared.localization;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.AcceptHeaderLocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

@Configuration
public class LocaleConfig {

    /**
     * Configure the locale resolver with Accept-Language header support
     * Automatically detects user's preferred language from HTTP header
     */
    @Bean
    public LocaleResolver localeResolver() {
        AcceptHeaderLocaleResolver resolver = new AcceptHeaderLocaleResolver();
        resolver.setDefaultLocale(Locale.ENGLISH);
        resolver.setSupportedLocales(getSupportedLocales());
        return resolver;
    }

    /**
     * Configure the message source for internationalization
     */
    @Bean
    public ResourceBundleMessageSource messageSource() {
        ResourceBundleMessageSource source = new ResourceBundleMessageSource();
        source.setBasenames("messages");
        source.setDefaultEncoding("UTF-8");
        source.setUseCodeAsDefaultMessage(true);
        source.setCacheSeconds(3600);
        return source;
    }

    /**
     * Configure locale change interceptor for URL-based locale switching
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
                new Locale("ar", "SA"));
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
