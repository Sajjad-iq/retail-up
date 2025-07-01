package com.retails.retail.auth.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * UserActivity entity for audit trail and activity logging
 * Maps to the frontend UserActivity interface
 */
@Entity
@Table(name = "user_activities", indexes = {
        @Index(name = "idx_activity_user", columnList = "userId"),
        @Index(name = "idx_activity_action", columnList = "action"),
        @Index(name = "idx_activity_timestamp", columnList = "timestamp"),
        @Index(name = "idx_activity_ip", columnList = "ipAddress")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserActivity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "id", updatable = false, nullable = false)
    private UUID id;

    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    @Enumerated(EnumType.STRING)
    @Column(name = "action", nullable = false)
    private UserAction action;

    @Size(max = 100, message = "Resource must not exceed 100 characters")
    @Column(name = "resource", length = 100)
    private String resource;

    @Size(max = 500, message = "Details must not exceed 500 characters")
    @Column(name = "details", length = 500)
    private String details;

    @Size(max = 45, message = "IP address must not exceed 45 characters")
    @Column(name = "ip_address", length = 45)
    private String ipAddress;

    @Size(max = 500, message = "User agent must not exceed 500 characters")
    @Column(name = "user_agent", length = 500)
    private String userAgent;

    @CreationTimestamp
    @Column(name = "timestamp", nullable = false, updatable = false)
    private LocalDateTime timestamp;

    /**
     * User action enumeration
     * Maps to the frontend UserAction type
     */
    public enum UserAction {
        LOGIN("login"),
        LOGOUT("logout"),
        CREATE_USER("create_user"),
        UPDATE_USER("update_user"),
        DELETE_USER("delete_user"),
        ACTIVATE_USER("activate_user"),
        DEACTIVATE_USER("deactivate_user"),
        LOCK_USER("lock_user"),
        UNLOCK_USER("unlock_user"),
        CREATE_ROLE("create_role"),
        UPDATE_ROLE("update_role"),
        DELETE_ROLE("delete_role"),
        ASSIGN_ROLE("assign_role"),
        REMOVE_ROLE("remove_role"),
        CHANGE_PASSWORD("change_password"),
        RESET_PASSWORD("reset_password"),
        FORCE_PASSWORD_CHANGE("force_password_change"),
        ACCESS_DENIED("access_denied");

        private final String value;

        UserAction(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static UserAction fromString(String value) {
            for (UserAction action : UserAction.values()) {
                if (action.value.equalsIgnoreCase(value)) {
                    return action;
                }
            }
            throw new IllegalArgumentException("Unknown user action: " + value);
        }
    }

    /**
     * Create activity for login
     */
    public static UserActivity createLoginActivity(UUID userId, String ipAddress, String userAgent) {
        return UserActivity.builder()
                .userId(userId)
                .action(UserAction.LOGIN)
                .ipAddress(ipAddress)
                .userAgent(userAgent)
                .build();
    }

    /**
     * Create activity for logout
     */
    public static UserActivity createLogoutActivity(UUID userId, String ipAddress, String userAgent) {
        return UserActivity.builder()
                .userId(userId)
                .action(UserAction.LOGOUT)
                .ipAddress(ipAddress)
                .userAgent(userAgent)
                .build();
    }

    /**
     * Create activity for user operations
     */
    public static UserActivity createUserActivity(UUID userId, UserAction action, String resource, String details,
            String ipAddress) {
        return UserActivity.builder()
                .userId(userId)
                .action(action)
                .resource(resource)
                .details(details)
                .ipAddress(ipAddress)
                .build();
    }
}