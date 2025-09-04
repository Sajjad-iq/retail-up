package com.sajjadkademm.retail.domain.user.model;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

import java.time.LocalDateTime;

import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.shared.enums.AccountType;

import jakarta.persistence.Column;
import jakarta.persistence.Index;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "users", indexes = {
        // Critical authentication indexes
        @Index(name = "idx_users_email", columnList = "email"),
        @Index(name = "idx_users_phone", columnList = "phone"),
        @Index(name = "idx_users_status", columnList = "status"),
        @Index(name = "idx_users_account_type", columnList = "account_type"),

        // Composite indexes for better performance
        @Index(name = "idx_users_auth", columnList = "email, phone, status"),
        @Index(name = "idx_users_status_type", columnList = "status, account_type"),
        @Index(name = "idx_users_created_at", columnList = "created_at"),
        @Index(name = "idx_users_last_login", columnList = "last_login_at"),

        // Advanced composite index for user management
        @Index(name = "idx_users_management", columnList = "status, account_type, created_at")
})
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "name", nullable = false)
    @NotBlank(message = "{user.name.empty}")
    private String name;

    @Column(name = "phone", nullable = true, unique = true)
    @Size(min = ValidationConstants.MIN_PHONE_LENGTH, max = ValidationConstants.MAX_PHONE_LENGTH)
    @JsonIgnore
    private String phone;

    @Column(name = "email", nullable = true)
    @Email(message = "{user.email.invalid.format}")
    private String email;

    @Column(name = "password", nullable = false)
    @NotBlank(message = "{auth.password.size}")
    @JsonIgnore
    private String password;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull(message = "{user.not.active}")
    private UserStatus status;

    @Column(name = "account_type", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull(message = "{invalid.user.data}")
    private AccountType accountType;

    @CreationTimestamp
    @Column(name = "created_at", nullable = true)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = true)
    private LocalDateTime updatedAt;

    @Column(name = "last_login_at", nullable = true)
    @JsonIgnore
    private LocalDateTime lastLoginAt;
}