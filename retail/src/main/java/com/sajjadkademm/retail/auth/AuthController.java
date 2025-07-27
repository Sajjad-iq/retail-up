package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.AuthResponse;
import com.sajjadkademm.retail.auth.dto.ChangePasswordRequest;
import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;
import com.sajjadkademm.retail.exceptions.BadRequestException;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {

    private final AuthService authService;

    /**
     * User login endpoint
     */
    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(@Valid @RequestBody LoginRequest request) {
        LoginResponse response = authService.login(request);
        return ResponseEntity.ok(response);
    }

    /**
     * User registration endpoint
     */
    @PostMapping("/register")
    public ResponseEntity<LoginResponse> register(@Valid @RequestBody RegisterRequest request) {
        LoginResponse response = authService.register(request);
        return ResponseEntity.ok(response);
    }

    /**
     * Check if user exists
     */
    @GetMapping("/exists")
    public ResponseEntity<Boolean> userExists(@RequestParam String emailOrPhone) {
        boolean exists = authService.userExists(emailOrPhone);
        return ResponseEntity.ok(exists);
    }

    /**
     * Change password endpoint
     */
    @PostMapping("/change-password")
    public ResponseEntity<AuthResponse> changePassword(@RequestBody ChangePasswordRequest request) {
        authService.changePassword(
                request.getUserId(),
                request.getOldPassword(),
                request.getNewPassword());

        return ResponseEntity.ok(AuthResponse.builder()
                .success(true)
                .message("Password changed successfully")
                .build());
    }

    /**
     * Validate JWT token endpoint
     */
    @GetMapping("/validate-token")
    public ResponseEntity<AuthResponse> validateToken(@RequestHeader("Authorization") String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            throw new BadRequestException("Invalid authorization header");
        }

        String token = authHeader.substring(7);
        boolean isValid = authService.validateToken(token);

        if (isValid) {
            return ResponseEntity.ok(AuthResponse.builder()
                    .success(true)
                    .message("Token is valid")
                    .build());
        } else {
            throw new BadRequestException("Token is invalid or expired");
        }
    }

}