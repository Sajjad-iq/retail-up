package com.sajjadkademm.retail.auth;

import com.sajjadkademm.retail.auth.dto.AuthResponse;
import com.sajjadkademm.retail.auth.dto.ChangePasswordRequest;
import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.dto.RegisterRequest;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@CrossOrigin(origins = "*")
public class AuthController {

    private final AuthService authService;

    /**
     * User login endpoint
     */
    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(@Valid @RequestBody LoginRequest request) {

        LoginResponse response = authService.login(request);

        if (response.getToken() != null) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * User registration endpoint
     */
    @PostMapping("/register")
    public ResponseEntity<LoginResponse> register(@Valid @RequestBody RegisterRequest request) {
        log.info("Registration request received for: {}", request.getEmail());

        LoginResponse response = authService.register(request);

        if (response.getToken() != null) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
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
        log.info("Change password request received for user: {}", request.getUserId());

        boolean success = authService.changePassword(
                request.getUserId(),
                request.getOldPassword(),
                request.getNewPassword());

        if (success) {
            return ResponseEntity.ok(AuthResponse.builder()
                    .success(true)
                    .message("Password changed successfully")
                    .build());
        } else {
            return ResponseEntity.badRequest().body(AuthResponse.builder()
                    .success(false)
                    .message("Failed to change password")
                    .build());
        }
    }

}