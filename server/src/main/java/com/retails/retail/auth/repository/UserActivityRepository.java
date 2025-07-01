package com.retails.retail.auth.repository;

import com.retails.retail.auth.entity.UserActivity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

/**
 * UserActivity repository for audit trail operations
 * Provides enhanced query methods for activity logging and analytics
 */
@Repository
public interface UserActivityRepository
                extends JpaRepository<UserActivity, UUID>, JpaSpecificationExecutor<UserActivity> {

        /**
         * Find activities by user ID
         */
        @Query("SELECT a FROM UserActivity a WHERE a.userId = :userId ORDER BY a.timestamp DESC")
        Page<UserActivity> findByUserId(@Param("userId") UUID userId, Pageable pageable);

        /**
         * Find activities by action
         */
        @Query("SELECT a FROM UserActivity a WHERE a.action = :action ORDER BY a.timestamp DESC")
        Page<UserActivity> findByAction(@Param("action") UserActivity.UserAction action, Pageable pageable);

        /**
         * Find activities between dates
         */
        @Query("SELECT a FROM UserActivity a WHERE a.timestamp BETWEEN :startDate AND :endDate ORDER BY a.timestamp DESC")
        Page<UserActivity> findByTimestampBetween(
                        @Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate,
                        Pageable pageable);

        /**
         * Find today's activities
         */
        @Query("SELECT a FROM UserActivity a WHERE DATE(a.timestamp) = CURRENT_DATE ORDER BY a.timestamp DESC")
        List<UserActivity> findTodaysActivities();

        /**
         * Find recent activities (limit 50)
         */
        @Query("SELECT a FROM UserActivity a ORDER BY a.timestamp DESC")
        Page<UserActivity> findRecentActivities(Pageable pageable);

        /**
         * Count activities by user and date range
         */
        @Query("SELECT COUNT(a) FROM UserActivity a WHERE a.userId = :userId AND a.timestamp BETWEEN :startDate AND :endDate")
        long countByUserIdAndTimestampBetween(
                        @Param("userId") UUID userId,
                        @Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

        /**
         * Count activities by action and date range
         */
        @Query("SELECT COUNT(a) FROM UserActivity a WHERE a.action = :action AND a.timestamp BETWEEN :startDate AND :endDate")
        long countByActionAndTimestampBetween(
                        @Param("action") UserActivity.UserAction action,
                        @Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

        /**
         * Find login activities by date range
         */
        @Query("SELECT a FROM UserActivity a WHERE a.action = 'LOGIN' AND a.timestamp BETWEEN :startDate AND :endDate ORDER BY a.timestamp DESC")
        List<UserActivity> findLoginActivitiesBetween(
                        @Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

        /**
         * Count unique users who logged in today
         */
        @Query("SELECT COUNT(DISTINCT a.userId) FROM UserActivity a WHERE a.action = 'LOGIN' AND DATE(a.timestamp) = CURRENT_DATE")
        long countUniqueLoginsToday();

        /**
         * Find activities by IP address
         */
        @Query("SELECT a FROM UserActivity a WHERE a.ipAddress = :ipAddress ORDER BY a.timestamp DESC")
        List<UserActivity> findByIpAddress(@Param("ipAddress") String ipAddress);

        /**
         * Find suspicious activities (multiple failed logins)
         */
        @Query("SELECT a FROM UserActivity a WHERE a.action = 'ACCESS_DENIED' AND a.timestamp >= :since AND a.ipAddress = :ipAddress")
        List<UserActivity> findSuspiciousActivities(
                        @Param("ipAddress") String ipAddress,
                        @Param("since") LocalDateTime since);

        /**
         * Get activity statistics for dashboard
         */
        @Query("SELECT a.action, COUNT(a) FROM UserActivity a WHERE DATE(a.timestamp) = CURRENT_DATE GROUP BY a.action")
        List<Object[]> getTodaysActivityStatistics();

        /**
         * Clean up old activities (older than specified date)
         */
        @Query("DELETE FROM UserActivity a WHERE a.timestamp < :cutoffDate")
        void deleteOldActivities(@Param("cutoffDate") LocalDateTime cutoffDate);

        /**
         * Find activities by user ID ordered by timestamp desc
         */
        @Query("SELECT a FROM UserActivity a WHERE a.userId = :userId ORDER BY a.timestamp DESC")
        Page<UserActivity> findByUserIdOrderByTimestampDesc(@Param("userId") UUID userId, Pageable pageable);

        /**
         * Find activities after timestamp
         */
        @Query("SELECT a FROM UserActivity a WHERE a.timestamp > :timestamp ORDER BY a.timestamp DESC")
        Page<UserActivity> findByTimestampAfter(@Param("timestamp") LocalDateTime timestamp, Pageable pageable);

        /**
         * Count activities after timestamp
         */
        @Query("SELECT COUNT(a) FROM UserActivity a WHERE a.timestamp > :timestamp")
        long countByTimestampAfter(@Param("timestamp") LocalDateTime timestamp);

        /**
         * Count activities by action
         */
        @Query("SELECT COUNT(a) FROM UserActivity a WHERE a.action = :action")
        long countByAction(@Param("action") UserActivity.UserAction action);

        /**
         * Delete activities before timestamp
         */
        @Query("DELETE FROM UserActivity a WHERE a.timestamp < :timestamp")
        long deleteByTimestampBefore(@Param("timestamp") LocalDateTime timestamp);
}