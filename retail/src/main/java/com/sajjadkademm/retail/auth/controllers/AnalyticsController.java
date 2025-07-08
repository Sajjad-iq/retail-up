package com.sajjadkademm.retail.auth.controllers;

import com.sajjadkademm.retail.auth.dto.UserResponse;
import com.sajjadkademm.retail.auth.services.PermissionService;
import com.sajjadkademm.retail.auth.services.UserActivityService;
import com.sajjadkademm.retail.auth.services.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * Analytics controller for user and permission analytics.
 * Provides endpoints for system analytics and reporting.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/analytics")
@RequiredArgsConstructor
@Tag(name = "Analytics", description = "Analytics and reporting endpoints")
@SecurityRequirement(name = "Bearer Authentication")
public class AnalyticsController {

    private final UserService userService;
    private final PermissionService permissionService;
    private final UserActivityService userActivityService;

    /**
     * Get user analytics dashboard
     */
    @GetMapping("/users")
    @PreAuthorize("hasAuthority('reports.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get user analytics", description = "Retrieve comprehensive user analytics data")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Analytics retrieved successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "403", description = "Forbidden")
    })
    public ResponseEntity<Map<String, Object>> getUserAnalytics() {
        // Get basic user counts
        long totalUsers = userService.getAllUsers(0, Integer.MAX_VALUE, "name", "asc").getTotalElements();
        long activeUsers = userService.getActiveUsersCount();
        long newUsersThisMonth = userService.getUsersCreatedThisMonth();
        long loginsToday = userService.getUsersLoggedInToday();

        // Get most active users
        List<UserResponse> mostActiveUsers = userService.getMostActiveUsers(10);

        // Get permission distribution
        List<Object[]> permissionDistribution = permissionService.getPermissionDistribution();

        // Get login activity for last 30 days
        LocalDateTime thirtyDaysAgo = LocalDateTime.now().minusDays(30);
        List<Object[]> loginActivity = userActivityService.getLoginActivityByDay(thirtyDaysAgo);

        Map<String, Object> analytics = Map.of(
                "totalUsers", totalUsers,
                "activeUsers", activeUsers,
                "loginsToday", loginsToday,
                "newUsersThisMonth", newUsersThisMonth,
                "mostActiveUsers", mostActiveUsers,
                "permissionDistribution", permissionDistribution,
                "loginActivity", loginActivity,
                "timestamp", LocalDateTime.now().toString());

        return ResponseEntity.ok(analytics);
    }

    /**
     * Get activity analytics
     */
    @GetMapping("/activities")
    @PreAuthorize("hasAuthority('reports.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get activity analytics", description = "Retrieve user activity analytics")
    public ResponseEntity<Map<String, Object>> getActivityAnalytics() {
        // Get activity count by action type
        List<Object[]> activityByAction = userActivityService.getActivityCountByAction();

        // Get today's activities
        List<com.sajjadkademm.retail.auth.entities.UserActivity> todaysActivities = userActivityService
                .getTodaysActivities();

        // Get most active users based on activity count
        List<Object[]> mostActiveUsers = userActivityService.getMostActiveUsers(10);

        // Get failed login attempts for last 24 hours
        LocalDateTime yesterday = LocalDateTime.now().minusHours(24);
        List<com.sajjadkademm.retail.auth.entities.UserActivity> failedLogins = userActivityService
                .getFailedLoginAttempts(yesterday);

        Map<String, Object> analytics = Map.of(
                "activityByAction", activityByAction,
                "todaysActivitiesCount", todaysActivities.size(),
                "mostActiveUsers", mostActiveUsers,
                "failedLoginsLast24Hours", failedLogins.size(),
                "timestamp", LocalDateTime.now().toString());

        return ResponseEntity.ok(analytics);
    }

    /**
     * Get permission analytics
     */
    @GetMapping("/permissions")
    @PreAuthorize("hasAuthority('reports.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get permission analytics", description = "Retrieve permission usage analytics")
    public ResponseEntity<Map<String, Object>> getPermissionAnalytics() {
        // Get all permissions
        List<com.sajjadkademm.retail.auth.dto.PermissionResponse> allPermissions = permissionService
                .getAllPermissions();

        // Get unassigned permissions
        List<com.sajjadkademm.retail.auth.dto.PermissionResponse> unassignedPermissions = permissionService
                .getUnassignedPermissions();

        // Get permission distribution
        List<Object[]> permissionDistribution = permissionService.getPermissionDistribution();

        // Count by category
        Map<String, Long> permissionsByCategory = allPermissions.stream()
                .collect(java.util.stream.Collectors.groupingBy(
                        p -> p.getCategory().name(),
                        java.util.stream.Collectors.counting()));

        Map<String, Object> analytics = Map.of(
                "totalPermissions", allPermissions.size(),
                "unassignedPermissions", unassignedPermissions.size(),
                "permissionsByCategory", permissionsByCategory,
                "permissionDistribution", permissionDistribution,
                "timestamp", LocalDateTime.now().toString());

        return ResponseEntity.ok(analytics);
    }

    /**
     * Get login activity for specific date range
     */
    @GetMapping("/login-activity")
    @PreAuthorize("hasAuthority('reports.view') or hasAuthority('admin.full_access')")
    @Operation(summary = "Get login activity", description = "Retrieve login activity for specific date range")
    public ResponseEntity<Map<String, Object>> getLoginActivity(
            @RequestParam(defaultValue = "30") int days) {

        LocalDateTime startDate = LocalDateTime.now().minusDays(days);

        // Get login activity by day
        List<Object[]> loginActivity = userActivityService.getLoginActivityByDay(startDate);

        // Get login activities details
        List<com.sajjadkademm.retail.auth.entities.UserActivity> loginDetails = userActivityService
                .getLoginActivitiesByDateRange(startDate, LocalDateTime.now());

        Map<String, Object> result = Map.of(
                "period", days + " days",
                "startDate", startDate.toString(),
                "endDate", LocalDateTime.now().toString(),
                "loginsByDay", loginActivity,
                "totalLogins", loginDetails.size(),
                "timestamp", LocalDateTime.now().toString());

        return ResponseEntity.ok(result);
    }

    /**
     * Get system health metrics
     */
    @GetMapping("/health")
    @PreAuthorize("hasAuthority('admin.full_access')")
    @Operation(summary = "Get system health", description = "Retrieve system health and performance metrics")
    public ResponseEntity<Map<String, Object>> getSystemHealth() {
        // Basic system metrics
        Runtime runtime = Runtime.getRuntime();
        long totalMemory = runtime.totalMemory();
        long freeMemory = runtime.freeMemory();
        long usedMemory = totalMemory - freeMemory;
        long maxMemory = runtime.maxMemory();

        // Database connection health (basic check)
        boolean dbHealthy = true;
        try {
            userService.getActiveUsersCount(); // Simple DB query to check connectivity
        } catch (Exception e) {
            dbHealthy = false;
        }

        Map<String, Object> health = Map.of(
                "status", dbHealthy ? "UP" : "DOWN",
                "memory", Map.of(
                        "total", totalMemory,
                        "free", freeMemory,
                        "used", usedMemory,
                        "max", maxMemory,
                        "usagePercentage", (double) usedMemory / totalMemory * 100),
                "database", Map.of(
                        "status", dbHealthy ? "UP" : "DOWN"),
                "timestamp", LocalDateTime.now().toString());

        return ResponseEntity.ok(health);
    }
}