package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.AuthAnalyticsDto;
import com.retails.retail.auth.service.UserActivityService;
import com.retails.retail.auth.service.UserService;
import com.retails.retail.auth.service.RoleService;
import com.retails.retail.common.dto.ApiResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * Authentication Analytics Controller
 * Handles authentication analytics, reporting, and dashboard data
 */
@RestController
@RequestMapping("/api/auth/analytics")
@RequiredArgsConstructor
@Slf4j
public class AuthAnalyticsController {

    private final UserService userService;
    private final UserActivityService userActivityService;
    private final RoleService roleService;

    /**
     * Get comprehensive authentication analytics
     */
    @GetMapping("/dashboard")
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<AuthAnalyticsDto>> getDashboardAnalytics() {
        log.info("Auth analytics dashboard request");

        try {
            // Get user statistics
            Map<String, Object> userStats = userService.getUserStatistics();
            AuthAnalyticsDto.UserStatistics userStatistics = AuthAnalyticsDto.UserStatistics.builder()
                    .totalUsers((Long) userStats.get("totalUsers"))
                    .activeUsers((Long) userStats.get("activeUsers"))
                    .inactiveUsers((Long) userStats.get("inactiveUsers"))
                    .lockedUsers((Long) userStats.get("lockedUsers"))
                    .pendingUsers((Long) userStats.get("pendingUsers"))
                    .usersRequiringPasswordChange((Long) userStats.get("usersRequiringPasswordChange"))
                    .newUsersThisMonth((Long) userStats.get("newUsersThisMonth"))
                    .newUsersToday((Long) userStats.get("newUsersToday"))
                    .build();

            // Get activity statistics
            Map<String, Object> activityStats = userActivityService.getActivityStatistics();
            AuthAnalyticsDto.ActivityStatistics activityStatistics = AuthAnalyticsDto.ActivityStatistics.builder()
                    .totalActivities((Long) activityStats.get("totalActivities"))
                    .activitiesToday((Long) activityStats.get("activitiesToday"))
                    .activitiesThisWeek((Long) activityStats.get("activitiesThisWeek"))
                    .topActions((Map<String, Long>) activityStats.get("actionBreakdown"))
                    .build();

            // Get role statistics
            Map<String, Object> roleStats = roleService.getRoleStatistics();

            // Build complete analytics
            AuthAnalyticsDto analytics = AuthAnalyticsDto.builder()
                    .userStatistics(userStatistics)
                    .activityStatistics(activityStatistics)
                    .generatedAt(LocalDateTime.now())
                    .build();

            return ResponseEntity.ok(ApiResponse.success(analytics));
        } catch (Exception e) {
            log.error("Error generating auth analytics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to generate analytics", "ANALYTICS_001"));
        }
    }

    /**
     * Get user statistics only
     */
    @GetMapping("/users")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getUserAnalytics() {
        log.info("User analytics request");

        try {
            Map<String, Object> stats = userService.getUserStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching user analytics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch user analytics", "ANALYTICS_002"));
        }
    }

    /**
     * Get activity statistics only
     */
    @GetMapping("/activities")
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getActivityAnalytics() {
        log.info("Activity analytics request");

        try {
            Map<String, Object> stats = userActivityService.getActivityStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching activity analytics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch activity analytics", "ANALYTICS_003"));
        }
    }

    /**
     * Get role statistics only
     */
    @GetMapping("/roles")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getRoleAnalytics() {
        log.info("Role analytics request");

        try {
            Map<String, Object> stats = roleService.getRoleStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching role analytics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch role analytics", "ANALYTICS_004"));
        }
    }

    /**
     * Get system health overview
     */
    @GetMapping("/health")
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getSystemHealth() {
        log.info("System health request");

        try {
            Map<String, Object> userStats = userService.getUserStatistics();
            Map<String, Object> activityStats = userActivityService.getActivityStatistics();
            Map<String, Object> roleStats = roleService.getRoleStatistics();

            Map<String, Object> health = Map.of(
                    "totalUsers", userStats.get("totalUsers"),
                    "activeUsers", userStats.get("activeUsers"),
                    "totalActivities", activityStats.get("totalActivities"),
                    "activitiesToday", activityStats.get("activitiesToday"),
                    "totalRoles", roleStats.get("totalRoles"),
                    "systemStatus", "HEALTHY",
                    "lastUpdated", LocalDateTime.now());

            return ResponseEntity.ok(ApiResponse.success(health));
        } catch (Exception e) {
            log.error("Error fetching system health", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch system health", "ANALYTICS_005"));
        }
    }
}