package com.sajjadkademm.retail.auth.controllers;

import com.sajjadkademm.retail.auth.entities.Permission;
import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.repositories.PermissionRepository;
import com.sajjadkademm.retail.auth.repositories.UserRepository;
import com.sajjadkademm.retail.auth.utils.AuthUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Health check controller for monitoring application status.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class HealthController {

    private final UserRepository userRepository;
    private final PermissionRepository permissionRepository;
    private final PasswordEncoder passwordEncoder;

    /**
     * Simple health check endpoint
     */
    @GetMapping("/health")
    public ResponseEntity<Map<String, Object>> health() {
        long userCount = userRepository.count();
        long permissionCount = permissionRepository.count();

        return ResponseEntity.ok(Map.of(
                "status", "UP",
                "timestamp", LocalDateTime.now(),
                "service", "Retail Auth Service",
                "userCount", userCount,
                "permissionCount", permissionCount,
                "adminExists", userRepository.existsByEmail("admin@retailup.com")));
    }

    /**
     * Create admin user manually (for debugging)
     * Remove this endpoint in production!
     */
    @PostMapping("/create-admin")
    public ResponseEntity<Map<String, Object>> createAdmin() {
        try {
            String adminEmail = "admin@retailup.com";

            if (userRepository.existsByEmail(adminEmail)) {
                return ResponseEntity.ok(Map.of(
                        "message", "Admin user already exists",
                        "email", adminEmail));
            }

            // Get all permissions for admin user
            List<Permission> allPermissions = permissionRepository.findAll();
            Set<Permission> adminPermissions = new HashSet<>(allPermissions);

            User adminUser = User.builder()
                    .id(AuthUtils.generateUserId())
                    .name("System Administrator")
                    .email(adminEmail)
                    .password(passwordEncoder.encode("admin123"))
                    .phone("+1234567890")
                    .department("IT")
                    .employeeId("ADMIN001")
                    .status(User.UserStatus.ACTIVE)
                    .mustChangePassword(true)
                    .permissions(adminPermissions)
                    .build();

            userRepository.save(adminUser);

            return ResponseEntity.ok(Map.of(
                    "message", "Admin user created successfully",
                    "email", adminEmail,
                    "password", "admin123",
                    "warning", "Change password immediately!"));

        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(Map.of(
                    "error", "Failed to create admin user",
                    "message", e.getMessage()));
        }
    }
}