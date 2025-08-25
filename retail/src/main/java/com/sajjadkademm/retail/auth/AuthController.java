package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.AuthResponse;
import com.sajjadkademm.retail.auth.dto.ChangePasswordRequest;
import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.auth.AuthErrorCode;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

/**
 * Authentication controller providing user authentication and authorization
 * endpoints.
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

    private final AuthService authService;
    private final LocalizedErrorService localizedErrorService;

    /**
     * User login endpoint
     */
    @Operation(summary = "User Login", description = "Authenticate a user with email/phone and password, returning JWT token and user information", operationId = "login")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Login successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class), examples = @ExampleObject(name = "Successful Login", value = """
                    {
                        "success": true,
                        "message": "Login successful",
                        "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
                        "user": {
                            "id": "123",
                            "email": "user@example.com",
                            "firstName": "John",
                            "lastName": "Doe"
                        }
                    }
                    """))),
            @ApiResponse(responseCode = "401", description = "Authentication failed - invalid credentials", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class), examples = @ExampleObject(name = "Authentication Failed", value = """
                    {
                        "error": "Authentication Failed",
                        "message": "Invalid email or password",
                        "status": 401,
                        "timestamp": "2024-12-19T10:30:00",
                        "path": "/api/auth/login"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class)))
    })
    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(
            @Parameter(description = "Login credentials", required = true, content = @Content(schema = @Schema(implementation = LoginRequest.class), examples = @ExampleObject(name = "Login Request", value = """
                    {
                        "emailOrPhone": "user@example.com",
                        "password": "password123"
                    }
                    """))) @Valid @RequestBody LoginRequest request) {
        LoginResponse response = authService.login(request);
        return ResponseEntity.ok(response);
    }

    /**
     * User registration endpoint
     */
    @Operation(summary = "User Registration", description = "Register a new user account with email, password, and personal information", operationId = "register")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Registration successful", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class), examples = @ExampleObject(name = "Successful Registration", value = """
                    {
                        "success": true,
                        "message": "Registration successful",
                        "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
                        "user": {
                            "id": "123",
                            "email": "newuser@example.com",
                            "firstName": "Jane",
                            "lastName": "Smith"
                        }
                    }
                    """))),
            @ApiResponse(responseCode = "409", description = "Conflict - user already exists", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class)))
    })
    @PostMapping("/register")
    public ResponseEntity<LoginResponse> register(
            @Parameter(description = "Registration information", required = true, content = @Content(schema = @Schema(implementation = RegisterRequest.class), examples = @ExampleObject(name = "Registration Request", value = """
                    {
                        "email": "newuser@example.com",
                        "password": "password123",
                        "firstName": "Jane",
                        "lastName": "Smith",
                        "phone": "+1234567890"
                    }
                    """))) @Valid @RequestBody RegisterRequest request) {
        LoginResponse response = authService.register(request);
        return ResponseEntity.ok(response);
    }

    /**
     * Check if user exists
     */
    @Operation(summary = "Check User Exists", description = "Check if a user exists by email or phone number", operationId = "userExists")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "User existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                    @ExampleObject(name = "User Exists", value = "true"),
                    @ExampleObject(name = "User Not Found", value = "false")
            }))
    })
    @GetMapping("/exists")
    public ResponseEntity<Boolean> userExists(
            @Parameter(description = "Email or phone number to check", required = true, example = "user@example.com") @RequestParam String emailOrPhone) {
        boolean exists = authService.userExists(emailOrPhone);
        return ResponseEntity.ok(exists);
    }

    /**
     * Change password endpoint
     */
    @Operation(summary = "Change Password", description = "Change user password with old password verification", operationId = "changePassword", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Password changed successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class), examples = @ExampleObject(name = "Password Changed", value = """
                    {
                        "success": true,
                        "message": "Password changed successfully"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - invalid old password or validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class)))
    })
    @PostMapping("/change-password")
    public ResponseEntity<AuthResponse> changePassword(
            @Parameter(description = "Password change request", required = true, content = @Content(schema = @Schema(implementation = ChangePasswordRequest.class), examples = @ExampleObject(name = "Change Password Request", value = """
                    {
                        "userId": "123",
                        "oldPassword": "oldpassword123",
                        "newPassword": "newpassword123"
                    }
                    """))) @RequestBody ChangePasswordRequest request) {
        authService.changePassword(
                request.getUserId(),
                request.getOldPassword(),
                request.getNewPassword());

        return ResponseEntity.ok(AuthResponse.builder()
                .success(true)
                .message(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_PASSWORD_CHANGED_SUCCESSFULLY.getMessage()))
                .build());
    }

    /**
     * Validate JWT token endpoint
     */
    @Operation(summary = "Validate JWT Token", description = "Validate if a JWT token is valid and not expired, returning user information if valid", operationId = "validateToken")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Token is valid", content = @Content(mediaType = "application/json", schema = @Schema(implementation = LoginResponse.class), examples = @ExampleObject(name = "Valid Token", value = """
                    {
                        "success": true,
                        "message": "Token is valid",
                        "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
                        "userId": "123",
                        "name": "John Doe",
                        "email": "user@example.com",
                        "phone": "+1234567890"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - invalid authorization header or token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = AuthResponse.class), examples = @ExampleObject(name = "Invalid Token", value = """
                    {
                        "error": "Bad Request",
                        "message": "Token is invalid or expired",
                        "status": 400,
                        "timestamp": "2024-12-19T10:30:00",
                        "path": "/api/auth/validate-token"
                    }
                    """)))
    })
    @GetMapping("/validate-token")
    public ResponseEntity<LoginResponse> validateToken(
            @Parameter(description = "Authorization header with Bearer token", required = true, example = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...") @RequestHeader("Authorization") String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            throw new BadRequestException(
                    localizedErrorService.getLocalizedMessage(
                            AuthErrorCode.AUTH_HEADER_INVALID.getMessage()));
        }

        String token = authHeader.substring(7);
        LoginResponse userInfo = authService.validateTokenAndGetUserInfo(token);

        if (userInfo != null) {
            return ResponseEntity.ok(userInfo);
        } else {
            throw new BadRequestException(
                    localizedErrorService.getLocalizedMessage(
                            AuthErrorCode.AUTH_TOKEN_INVALID.getMessage()));
        }
    }

}