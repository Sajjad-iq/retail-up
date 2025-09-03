package com.sajjadkademm.retail.application.config.web;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;

/**
 * Web MVC configuration for the retail application.
 * Handles web-specific configurations like interceptors and view resolvers.
 * 
 * Separation of Concerns:
 * - Only handles web MVC configurations
 * - Separated from security configuration for better maintainability
 * - Manages locale change interceptors for internationalization
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    private final LocaleChangeInterceptor localeChangeInterceptor;

    @Autowired
    public WebMvcConfig(LocaleChangeInterceptor localeChangeInterceptor) {
        this.localeChangeInterceptor = localeChangeInterceptor;
    }

    /**
     * Register locale change interceptor for dynamic locale switching.
     * Allows users to change language using ?lang=en or ?lang=ar parameter.
     */
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        // Add locale change interceptor for internationalization support
        registry.addInterceptor(localeChangeInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns("/api-docs/**", "/swagger-ui/**", "/actuator/**");
    }
}