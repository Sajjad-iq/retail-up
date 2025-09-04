package com.sajjadkademm.retail.application.controllers.auth;

import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.application.dto.auth.ChangePasswordRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.application.dto.auth.RegisterRequest;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.auth.commands.*;
import com.sajjadkademm.retail.domain.auth.queries.*;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.servlet.http.HttpServletRequest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

/**
 * Authentication controller providing user authentication and authorization
 * endpoints.
 * All operations are scoped to the current authenticated user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Authentication", description = "Authentication and authorization endpoints")
public class AuthController {

        private final CommandBus commandBus;
        private final QueryBus queryBus;

        /**
         * User login endpoint
         */
        @Operation(summary = "User Login", description = "Authenticate a user with email/phone and password")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Login successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "401", description = "Authentication failed"),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @PostMapping("/login")
        public LoginResponse login(
                        @Parameter(description = "Login credentials", required = true) @Valid @RequestBody LoginRequest request,
                        HttpServletRequest httpRequest) throws Exception {
                
                // CQRS: Use command for login operation
                LoginCommand command = LoginCommand.builder()
                        .request(request)
                        .clientIp(getClientIpAddress(httpRequest))
                        .userAgent(httpRequest.getHeader("User-Agent"))
                        .build();
                
                return commandBus.execute(command);
        }

        /**
         * User registration endpoint
         */
        @Operation(summary = "User Registration", description = "Register a new user account")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Registration successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "409", description = "User already exists"),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @PostMapping("/register")
        public LoginResponse register(
                        @Parameter(description = "Registration information", required = true) @Valid @RequestBody RegisterRequest request,
                        HttpServletRequest httpRequest) throws Exception {
                
                // CQRS: Use command for registration operation
                RegisterCommand command = RegisterCommand.builder()
                        .request(request)
                        .clientIp(getClientIpAddress(httpRequest))
                        .userAgent(httpRequest.getHeader("User-Agent"))
                        .build();
                
                return commandBus.execute(command);
        }

        /**
         * Get current user profile endpoint
         */
        @Operation(summary = "Get Current User Profile", description = "Retrieve the current authenticated user's profile", security = @SecurityRequirement(name = "Bearer Authentication"))
        @ApiResponse(responseCode = "200", description = "User profile retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = User.class)))
        @GetMapping("/me")
        public User getCurrentUserProfile() throws Exception {
                
                // CQRS: Use query for read operation
                GetCurrentUserQuery query = GetCurrentUserQuery.builder()
                        .userId(SecurityUtils.getCurrentUserId())
                        .build();
                
                return queryBus.execute(query);
        }

        /**
         * Check if user exists
         */
        @Operation(summary = "Check User Exists", description = "Check if a user exists by email or phone number")
        @ApiResponse(responseCode = "200", description = "User existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean")))
        @GetMapping("/exists")
        public boolean userExists(
                        @Parameter(description = "Email or phone number to check", required = true) @RequestParam String emailOrPhone) throws Exception {
                
                // CQRS: Use query for read operation
                UserExistsQuery query = UserExistsQuery.builder()
                        .emailOrPhone(emailOrPhone)
                        .build();
                
                return queryBus.execute(query);
        }

        /**
         * Change current user password endpoint
         */
        @Operation(summary = "Change Current User Password", description = "Change the current authenticated user's password", security = @SecurityRequirement(name = "Bearer Authentication"))
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request"),
                        @ApiResponse(responseCode = "401", description = "Unauthorized")
        })
        @PostMapping("/change-password")
        public AuthResponse changePassword(
                        @Parameter(description = "Password change request", required = true) @Valid @RequestBody ChangePasswordRequest request,
                        HttpServletRequest httpRequest) throws Exception {
                
                // CQRS: Use command for password change operation
                ChangePasswordCommand command = ChangePasswordCommand.builder()
                        .userId(SecurityUtils.getCurrentUserId())
                        .oldPassword(request.getOldPassword())
                        .newPassword(request.getNewPassword())
                        .clientIp(getClientIpAddress(httpRequest))
                        .userAgent(httpRequest.getHeader("User-Agent"))
                        .build();
                
                return commandBus.execute(command);
        }

        /**
         * Validate JWT token endpoint
         */
        @Operation(summary = "Validate JWT Token", description = "Validate if a JWT token is valid and return user information")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Token is valid", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request")
        })
        @GetMapping("/validate-token")
        public LoginResponse validateToken(HttpServletRequest httpRequest) throws Exception {
                
                // CQRS: Use query for token validation operation
                ValidateTokenQuery query = ValidateTokenQuery.builder()
                        .userId(SecurityUtils.getCurrentUserId())
                        .clientIp(getClientIpAddress(httpRequest))
                        .userAgent(httpRequest.getHeader("User-Agent"))
                        .build();
                
                return queryBus.execute(query);
        }

        /**
         * Helper method to get client IP address
         */
        private String getClientIpAddress(HttpServletRequest request) {
                String xForwardedFor = request.getHeader("X-Forwarded-For");
                if (xForwardedFor != null && !xForwardedFor.isEmpty() && !"unknown".equalsIgnoreCase(xForwardedFor)) {
                        return xForwardedFor.split(",")[0].trim();
                }
                
                String xRealIp = request.getHeader("X-Real-IP");
                if (xRealIp != null && !xRealIp.isEmpty() && !"unknown".equalsIgnoreCase(xRealIp)) {
                        return xRealIp;
                }
                
                return request.getRemoteAddr();
        }
}