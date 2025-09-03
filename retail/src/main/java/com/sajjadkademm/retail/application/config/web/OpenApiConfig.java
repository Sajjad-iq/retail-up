package com.sajjadkademm.retail.application.config.web;

import com.sajjadkademm.retail.shared.constants.SecurityConstants;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * OpenAPI/Swagger configuration for API documentation.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Configuration
public class OpenApiConfig {

        @Value("${server.port}")
        private String serverPort;

        @Bean
        public OpenAPI customOpenAPI() {
                return new OpenAPI()
                                .info(new Info()
                                                .title("Retail Management System API")
                                                .description("""
                                                                Comprehensive REST API for Retail Management System.

                                                                ## Features
                                                                - **Authentication & Authorization**: User login, registration, and JWT token management
                                                                - **Organization Management**: Multi-tenant organization setup and management
                                                                - **System Settings**: Global system configuration management
                                                                - **POS Settings**: Point of Sale system configuration
                                                                - **Inventory Settings**: Inventory management system configuration

                                                                ## Authentication
                                                                Most endpoints require Bearer token authentication. Include the JWT token in the Authorization header:
                                                                ```
                                                                Authorization: Bearer <your-jwt-token>
                                                                ```

                                                                ## API Structure
                                                                - `/api/auth/*` - Authentication and authorization endpoints
                                                                - `/api/organizations/*` - Organization management endpoints
                                                                - `/api/settings/*` - Settings management endpoints
                                                                - `/health` - Application health check
                                                                """)
                                                .version("1.0.0")
                                                .contact(new Contact()
                                                                .name("Sajjad Kadem")
                                                                .email("sajjad.kadem@example.com")
                                                                .url("https://github.com/sajjadkadem"))
                                                .license(new License()
                                                                .name("MIT License")
                                                                .url("https://opensource.org/licenses/MIT")))
                                .servers(List.of(
                                                new Server()
                                                                .url("http://localhost:" + serverPort)
                                                                .description("Development Server"),
                                                new Server()
                                                                .url("https://api.retail-up.com")
                                                                .description("Production Server")))
                                .addSecurityItem(new SecurityRequirement().addList("Bearer Authentication"))
                                .components(new Components()
                                                .addSecuritySchemes("Bearer Authentication",
                                                                new SecurityScheme()
                                                                                .type(SecurityScheme.Type.HTTP)
                                                                                .scheme("bearer")
                                                                                .bearerFormat("JWT")
                                                                                .in(SecurityScheme.In.HEADER)
                                                                                .name(SecurityConstants.JWT_HEADER)
                                                                                .description("JWT token for authentication")));
        }
}