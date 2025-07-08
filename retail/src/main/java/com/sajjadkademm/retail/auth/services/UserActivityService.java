package com.sajjadkademm.retail.auth.services;

import com.sajjadkademm.retail.auth.entities.UserActivity;
import com.sajjadkademm.retail.auth.repositories.UserActivityRepository;
import com.sajjadkademm.retail.auth.utils.AuthUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Service for user activity logging and analytics.
 * Handles activity tracking and audit trail functionality.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class UserActivityService {

    private final UserActivityRepository userActivityRepository;

    /**
     * Log user activity
     */
    @Transactional
    public void logActivity(String userId, UserActivity.UserAction action, String resource,
            String details, String ipAddress, String userAgent) {
        try {
            UserActivity activity = UserActivity.builder()
                    .id(AuthUtils.generateActivityId())
                    .userId(userId)
                    .action(action)
                    .resource(resource)
                    .details(details)
                    .ipAddress(ipAddress)
                    .userAgent(userAgent)
                    .build();

            userActivityRepository.save(activity);
            log.debug("Activity logged: {} for user: {}", action, userId);
        } catch (Exception e) {
            log.error("Failed to log activity: {} for user: {} - {}", action, userId, e.getMessage());
        }
    }

    /**
     * Get activities by user ID
     */
    @Transactional(readOnly = true)
    public Page<UserActivity> getActivitiesByUserId(String userId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return userActivityRepository.findByUserId(userId, pageable);
    }

    /**
     * Get recent activities
     */
    @Transactional(readOnly = true)
    public Page<UserActivity> getRecentActivities(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        return userActivityRepository.findAllByOrderByTimestampDesc(pageable);
    }

    /**
     * Get today's activities
     */
    @Transactional(readOnly = true)
    public List<UserActivity> getTodaysActivities() {
        LocalDateTime startOfDay = LocalDateTime.now().toLocalDate().atStartOfDay();
        LocalDateTime endOfDay = startOfDay.plusDays(1);
        return userActivityRepository.findTodaysActivities(startOfDay, endOfDay);
    }

    /**
     * Get activities by action
     */
    @Transactional(readOnly = true)
    public List<UserActivity> getActivitiesByAction(UserActivity.UserAction action) {
        return userActivityRepository.findByAction(action);
    }

    /**
     * Get activities in date range
     */
    @Transactional(readOnly = true)
    public List<UserActivity> getActivitiesInDateRange(LocalDateTime startDate, LocalDateTime endDate) {
        return userActivityRepository.findByTimestampBetween(startDate, endDate);
    }

    /**
     * Get login activities by date range
     */
    @Transactional(readOnly = true)
    public List<UserActivity> getLoginActivitiesByDateRange(LocalDateTime startDate, LocalDateTime endDate) {
        return userActivityRepository.findLoginActivitiesByDateRange(startDate, endDate);
    }

    /**
     * Get login activity by day
     */
    @Transactional(readOnly = true)
    public List<Object[]> getLoginActivityByDay(LocalDateTime startDate) {
        return userActivityRepository.getLoginActivityByDay(startDate);
    }

    /**
     * Get activity count by action
     */
    @Transactional(readOnly = true)
    public List<Object[]> getActivityCountByAction() {
        return userActivityRepository.getActivityCountByAction();
    }

    /**
     * Get most active users
     */
    @Transactional(readOnly = true)
    public List<Object[]> getMostActiveUsers(int limit) {
        Pageable pageable = PageRequest.of(0, limit);
        return userActivityRepository.findMostActiveUsers(pageable);
    }

    /**
     * Search activities
     */
    @Transactional(readOnly = true)
    public List<UserActivity> searchActivities(String query) {
        return userActivityRepository.searchActivities(query);
    }

    /**
     * Get failed login attempts
     */
    @Transactional(readOnly = true)
    public List<UserActivity> getFailedLoginAttempts(LocalDateTime since) {
        return userActivityRepository.findFailedLoginAttempts(since);
    }

    /**
     * Count failed login attempts by IP
     */
    @Transactional(readOnly = true)
    public long countFailedLoginAttemptsByIp(String ipAddress, LocalDateTime since) {
        return userActivityRepository.countFailedLoginAttemptsByIp(ipAddress, since);
    }

    /**
     * Clean up old activities
     */
    @Transactional
    public void cleanupOldActivities(LocalDateTime cutoffDate) {
        try {
            userActivityRepository.deleteByTimestampBefore(cutoffDate);
            log.info("Cleaned up activities older than: {}", cutoffDate);
        } catch (Exception e) {
            log.error("Failed to cleanup old activities: {}", e.getMessage());
        }
    }
}