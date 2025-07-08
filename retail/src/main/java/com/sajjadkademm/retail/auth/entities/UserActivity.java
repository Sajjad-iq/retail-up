package com.sajjadkademm.retail.auth.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

/**
 * UserActivity entity for logging user actions and system events.
 * Provides audit trail for security and compliance purposes.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Entity
@Table(name = "user_activities", indexes = {
        @Index(name = "idx_activity_user_id", columnList = "user_id"),
        @Index(name = "idx_activity_action", columnList = "action"),
        @Index(name = "idx_activity_timestamp", columnList = "timestamp"),
        @Index(name = "idx_activity_user_timestamp", columnList = "user_id, timestamp")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserActivity {

    /**
     * Unique activity identifier
     */
    @Id
    @Column(name = "id", length = 50)
    private String id;

    /**
     * User who performed the action
     */
    @Column(name = "user_id", nullable = false, length = 50)
    private String userId;

    /**
     * User details (fetched via service layer)
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    /**
     * Action performed
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "action", nullable = false, length = 30)
    private UserAction action;

    /**
     * Resource affected
     */
    @Column(name = "resource", length = 200)
    private String resource;

    /**
     * Additional details about the action
     */
    @Column(name = "details", length = 1000)
    private String details;

    /**
     * IP address from which action was performed
     */
    @Column(name = "ip_address", length = 45)
    private String ipAddress;

    /**
     * User agent string
     */
    @Column(name = "user_agent", length = 500)
    private String userAgent;

    /**
     * Activity timestamp
     */
    @CreationTimestamp
    @Column(name = "timestamp", nullable = false, updatable = false)
    private LocalDateTime timestamp;

    /**
     * User action enumeration
     */
    public enum UserAction {
        LOGIN("login"),
        LOGOUT("logout"),
        CREATE_USER("create_user"),
        UPDATE_USER("update_user"),
        DELETE_USER("delete_user"),
        CHANGE_PASSWORD("change_password"),
        RESET_PASSWORD("reset_password"),
        ACCESS_DENIED("access_denied"),
        PERMISSION_GRANTED("permission_granted"),
        PERMISSION_REVOKED("permission_revoked"),
        ACCOUNT_LOCKED("account_locked"),
        ACCOUNT_UNLOCKED("account_unlocked");

        private final String value;

        UserAction(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static UserAction fromValue(String value) {
            for (UserAction action : values()) {
                if (action.value.equals(value)) {
                    return action;
                }
            }
            throw new IllegalArgumentException("Unknown user action: " + value);
        }
    }
}