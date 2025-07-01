package com.retails.retail.auth.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * PasswordResetRequest entity for handling password reset operations
 * Maps to the frontend PasswordResetRequest interface
 */
@Entity
@Table(name = "password_reset_requests", indexes = {
        @Index(name = "idx_reset_email", columnList = "email"),
        @Index(name = "idx_reset_token", columnList = "token", unique = true),
        @Index(name = "idx_reset_expires", columnList = "expiresAt"),
        @Index(name = "idx_reset_used", columnList = "used")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PasswordResetRequest {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "id", updatable = false, nullable = false)
    private UUID id;

    @NotBlank(message = "Email is required")
    @Email(message = "Please enter a valid email address")
    @Column(name = "email", nullable = false, length = 100)
    private String email;

    @NotBlank(message = "Reset token is required")
    @Column(name = "token", nullable = false, unique = true, length = 100)
    private String token;

    @Column(name = "expires_at", nullable = false)
    private LocalDateTime expiresAt;

    @Column(name = "used", nullable = false)
    @Builder.Default
    private Boolean used = false;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "used_at")
    private LocalDateTime usedAt;

    /**
     * Check if the reset request is valid
     */
    public boolean isValid() {
        return !used && LocalDateTime.now().isBefore(expiresAt);
    }

    /**
     * Check if the reset request is expired
     */
    public boolean isExpired() {
        return LocalDateTime.now().isAfter(expiresAt);
    }

    /**
     * Mark the reset request as used
     */
    public void markAsUsed() {
        this.used = true;
        this.usedAt = LocalDateTime.now();
    }

    /**
     * Generate expiry time (24 hours from now)
     */
    public static LocalDateTime generateExpiryTime() {
        return LocalDateTime.now().plusHours(24);
    }
}