package com.sajjadkademm.retail.auth.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

/**
 * AuthSession entity for managing user authentication sessions.
 * Tracks active user sessions and handles session validation.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Entity
@Table(name = "auth_sessions", indexes = {
        @Index(name = "idx_session_token", columnList = "token"),
        @Index(name = "idx_session_user_id", columnList = "user_id"),
        @Index(name = "idx_session_expires_at", columnList = "expires_at"),
        @Index(name = "idx_session_active", columnList = "is_active")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuthSession {

    /**
     * Unique session identifier
     */
    @Id
    @Column(name = "id", length = 50)
    private String id;

    /**
     * Session token (JWT)
     */
    @Column(name = "token", nullable = false, unique = true, length = 3000)
    private String token;

    /**
     * User ID associated with this session
     */
    @Column(name = "user_id", nullable = false, length = 50)
    private String userId;

    /**
     * User associated with this session
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    /**
     * Session expiry date
     */
    @Column(name = "expires_at", nullable = false)
    private LocalDateTime expiresAt;

    /**
     * Session creation date
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * Whether the session is still active
     */
    @Column(name = "is_active", nullable = false)
    private Boolean isActive = true;

    /**
     * IP address from which session was created
     */
    @Column(name = "ip_address", length = 45)
    private String ipAddress;

    /**
     * User agent string
     */
    @Column(name = "user_agent", length = 500)
    private String userAgent;

    /**
     * When the session was last accessed
     */
    @Column(name = "last_accessed_at")
    private LocalDateTime lastAccessedAt;

    /**
     * When the session was terminated
     */
    @Column(name = "terminated_at")
    private LocalDateTime terminatedAt;

    /**
     * Reason for session termination
     */
    @Column(name = "termination_reason", length = 100)
    private String terminationReason;

    /**
     * Check if session is expired
     */
    public boolean isExpired() {
        return LocalDateTime.now().isAfter(this.expiresAt);
    }

    /**
     * Check if session is valid (active and not expired)
     */
    public boolean isValid() {
        return this.isActive && !isExpired();
    }

    /**
     * Terminate this session
     */
    public void terminate(String reason) {
        this.isActive = false;
        this.terminatedAt = LocalDateTime.now();
        this.terminationReason = reason;
    }

    /**
     * Update last accessed timestamp
     */
    public void updateLastAccessed() {
        this.lastAccessedAt = LocalDateTime.now();
    }
}