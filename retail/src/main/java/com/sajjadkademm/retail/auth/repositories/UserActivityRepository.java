package com.sajjadkademm.retail.auth.repositories;

import com.sajjadkademm.retail.auth.entities.UserActivity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository interface for UserActivity entity.
 * Provides data access methods for user activity logging and analytics.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Repository
public interface UserActivityRepository extends JpaRepository<UserActivity, String> {

        /**
         * Find activities by user ID
         */
        List<UserActivity> findByUserId(String userId);

        /**
         * Find activities by user ID with pagination
         */
        Page<UserActivity> findByUserId(String userId, Pageable pageable);

        /**
         * Find activities by action type
         */
        List<UserActivity> findByAction(UserActivity.UserAction action);

        /**
         * Find activities by user ID and action
         */
        List<UserActivity> findByUserIdAndAction(String userId, UserActivity.UserAction action);

        /**
         * Find activities in date range
         */
        List<UserActivity> findByTimestampBetween(LocalDateTime startDate, LocalDateTime endDate);

        /**
         * Find activities after specific date
         */
        List<UserActivity> findByTimestampAfter(LocalDateTime timestamp);

        /**
         * Find activities by user and date range
         */
        List<UserActivity> findByUserIdAndTimestampBetween(String userId,
                        LocalDateTime startDate,
                        LocalDateTime endDate);

        /**
         * Find today's activities
         */
        @Query("SELECT a FROM UserActivity a WHERE a.timestamp >= :startOfDay AND a.timestamp < :endOfDay")
        List<UserActivity> findTodaysActivities(@Param("startOfDay") LocalDateTime startOfDay,
                        @Param("endOfDay") LocalDateTime endOfDay);

        /**
         * Find activities by IP address
         */
        List<UserActivity> findByIpAddress(String ipAddress);

        /**
         * Count activities by action type
         */
        long countByAction(UserActivity.UserAction action);

        /**
         * Count activities for user
         */
        long countByUserId(String userId);

        /**
         * Count activities in date range
         */
        long countByTimestampBetween(LocalDateTime startDate, LocalDateTime endDate);

        /**
         * Find login activities by date range
         */
        @Query("SELECT a FROM UserActivity a WHERE a.action = 'LOGIN' AND " +
                        "a.timestamp BETWEEN :startDate AND :endDate ORDER BY a.timestamp DESC")
        List<UserActivity> findLoginActivitiesByDateRange(@Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

        /**
         * Get login activity count by day
         */
        @Query("SELECT DATE(a.timestamp) as date, COUNT(a) as count FROM UserActivity a " +
                        "WHERE a.action = 'LOGIN' AND a.timestamp >= :startDate " +
                        "GROUP BY DATE(a.timestamp) ORDER BY date")
        List<Object[]> getLoginActivityByDay(@Param("startDate") LocalDateTime startDate);

        /**
         * Get activity count by action type
         */
        @Query("SELECT a.action, COUNT(a) FROM UserActivity a GROUP BY a.action")
        List<Object[]> getActivityCountByAction();

        /**
         * Find most active users
         */
        @Query("SELECT a.userId, COUNT(a) as activityCount FROM UserActivity a " +
                        "GROUP BY a.userId ORDER BY activityCount DESC")
        List<Object[]> findMostActiveUsers(Pageable pageable);

        /**
         * Find recent activities with pagination
         */
        Page<UserActivity> findAllByOrderByTimestampDesc(Pageable pageable);

        /**
         * Search activities by details or resource
         */
        @Query("SELECT a FROM UserActivity a WHERE " +
                        "LOWER(a.details) LIKE LOWER(CONCAT('%', :query, '%')) OR " +
                        "LOWER(a.resource) LIKE LOWER(CONCAT('%', :query, '%'))")
        List<UserActivity> searchActivities(@Param("query") String query);

        /**
         * Delete old activities
         */
        void deleteByTimestampBefore(LocalDateTime timestamp);

        /**
         * Find failed login attempts
         */
        @Query("SELECT a FROM UserActivity a WHERE a.action = 'ACCESS_DENIED' AND " +
                        "a.details LIKE '%login%' AND a.timestamp >= :since")
        List<UserActivity> findFailedLoginAttempts(@Param("since") LocalDateTime since);

        /**
         * Count failed login attempts by IP
         */
        @Query("SELECT COUNT(a) FROM UserActivity a WHERE a.action = 'ACCESS_DENIED' AND " +
                        "a.ipAddress = :ipAddress AND a.timestamp >= :since")
        long countFailedLoginAttemptsByIp(@Param("ipAddress") String ipAddress,
                        @Param("since") LocalDateTime since);
}