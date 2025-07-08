package com.sajjadkademm.retail.auth.repositories;

import com.sajjadkademm.retail.auth.entities.AuthSession;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository interface for AuthSession entity.
 * Provides data access methods for session management operations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Repository
public interface AuthSessionRepository extends JpaRepository<AuthSession, String> {

    /**
     * Find session by token
     */
    Optional<AuthSession> findByToken(String token);

    /**
     * Find active sessions by user ID
     */
    List<AuthSession> findByUserIdAndIsActiveTrue(String userId);

    /**
     * Find all sessions by user ID
     */
    List<AuthSession> findByUserId(String userId);

    /**
     * Find active sessions
     */
    List<AuthSession> findByIsActiveTrue();

    /**
     * Find expired sessions
     */
    @Query("SELECT s FROM AuthSession s WHERE s.expiresAt < :currentTime")
    List<AuthSession> findExpiredSessions(@Param("currentTime") LocalDateTime currentTime);

    /**
     * Find active and non-expired sessions
     */
    @Query("SELECT s FROM AuthSession s WHERE s.isActive = true AND s.expiresAt > :currentTime")
    List<AuthSession> findValidSessions(@Param("currentTime") LocalDateTime currentTime);

    /**
     * Find sessions by IP address
     */
    List<AuthSession> findByIpAddress(String ipAddress);

    /**
     * Find sessions created after specific time
     */
    List<AuthSession> findByCreatedAtAfter(LocalDateTime createdAt);

    /**
     * Count active sessions for user
     */
    long countByUserIdAndIsActiveTrue(String userId);

    /**
     * Count total sessions for user
     */
    long countByUserId(String userId);

    /**
     * Terminate all active sessions for user
     */
    @Modifying
    @Query("UPDATE AuthSession s SET s.isActive = false, s.terminatedAt = :terminatedAt, " +
            "s.terminationReason = :reason WHERE s.userId = :userId AND s.isActive = true")
    void terminateAllUserSessions(@Param("userId") String userId,
            @Param("terminatedAt") LocalDateTime terminatedAt,
            @Param("reason") String reason);

    /**
     * Terminate specific session
     */
    @Modifying
    @Query("UPDATE AuthSession s SET s.isActive = false, s.terminatedAt = :terminatedAt, " +
            "s.terminationReason = :reason WHERE s.id = :sessionId")
    void terminateSession(@Param("sessionId") String sessionId,
            @Param("terminatedAt") LocalDateTime terminatedAt,
            @Param("reason") String reason);

    /**
     * Update last accessed time
     */
    @Modifying
    @Query("UPDATE AuthSession s SET s.lastAccessedAt = :lastAccessedAt WHERE s.id = :sessionId")
    void updateLastAccessedTime(@Param("sessionId") String sessionId,
            @Param("lastAccessedAt") LocalDateTime lastAccessedAt);

    /**
     * Delete expired sessions
     */
    void deleteByExpiresAtBefore(LocalDateTime expiredTime);

    /**
     * Delete inactive sessions older than specific time
     */
    @Modifying
    @Query("DELETE FROM AuthSession s WHERE s.isActive = false AND s.terminatedAt < :cutoffTime")
    void deleteOldInactiveSessions(@Param("cutoffTime") LocalDateTime cutoffTime);

    /**
     * Find sessions that haven't been accessed recently
     */
    @Query("SELECT s FROM AuthSession s WHERE s.isActive = true AND " +
            "(s.lastAccessedAt IS NULL OR s.lastAccessedAt < :cutoffTime)")
    List<AuthSession> findStaleActiveSessions(@Param("cutoffTime") LocalDateTime cutoffTime);

    /**
     * Count sessions by status
     */
    long countByIsActive(Boolean isActive);

    /**
     * Find concurrent sessions for user
     */
    @Query("SELECT s FROM AuthSession s WHERE s.userId = :userId AND s.isActive = true " +
            "ORDER BY s.createdAt DESC")
    List<AuthSession> findConcurrentUserSessions(@Param("userId") String userId);

    /**
     * Get session statistics
     */
    @Query("SELECT s.isActive, COUNT(s) FROM AuthSession s GROUP BY s.isActive")
    List<Object[]> getSessionStatistics();
}