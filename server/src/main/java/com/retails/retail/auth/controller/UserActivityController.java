package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.UserActivitySearchRequest;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.service.UserActivityService;
import com.retails.retail.common.dto.ApiResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * User Activity Controller
 * Handles user activity monitoring, search, and analytics
 */
@RestController
@RequestMapping("/api/auth/activities")
@RequiredArgsConstructor
@Slf4j
public class UserActivityController {

    private final UserActivityService userActivityService;

    /**
     * Get user activities with pagination and filtering
     */
    @GetMapping
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<Page<UserActivity>>> getActivities(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size,
            @RequestParam(defaultValue = "timestamp") String sortBy,
            @RequestParam(defaultValue = "DESC") String sortDirection,
            @RequestParam(required = false) String query,
            @RequestParam(required = false) String actions,
            @RequestParam(required = false) String userIds) {

        log.info("Get activities request - page: {}, size: {}, query: {}", page, size, query);

        try {
            UserActivitySearchRequest searchRequest = new UserActivitySearchRequest();
            searchRequest.setPage(page);
            searchRequest.setSize(size);
            searchRequest.setSortBy(sortBy);
            searchRequest.setSortDirection(sortDirection);
            searchRequest.setQuery(query);

            Page<UserActivity> activities = userActivityService.getActivities(searchRequest);
            return ResponseEntity.ok(ApiResponse.success(activities));
        } catch (Exception e) {
            log.error("Error fetching activities", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch activities", "ACTIVITY_001"));
        }
    }

    /**
     * Get activities for specific user
     */
    @GetMapping("/user/{userId}")
    @PreAuthorize("hasPermission('admin', 'audit') or #userId == authentication.principal.id")
    public ResponseEntity<ApiResponse<Page<UserActivity>>> getActivitiesByUser(
            @PathVariable UUID userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size) {

        log.info("Get activities for user: {}", userId);

        try {
            Page<UserActivity> activities = userActivityService.getActivitiesByUser(userId, page, size);
            return ResponseEntity.ok(ApiResponse.success(activities));
        } catch (Exception e) {
            log.error("Error fetching activities for user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch user activities", "ACTIVITY_002"));
        }
    }

    /**
     * Get recent activities (last 24 hours)
     */
    @GetMapping("/recent")
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<List<UserActivity>>> getRecentActivities(
            @RequestParam(defaultValue = "100") int limit) {

        log.info("Get recent activities request - limit: {}", limit);

        try {
            List<UserActivity> activities = userActivityService.getRecentActivities(limit);
            return ResponseEntity.ok(ApiResponse.success(activities));
        } catch (Exception e) {
            log.error("Error fetching recent activities", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch recent activities", "ACTIVITY_003"));
        }
    }

    /**
     * Get activity statistics
     */
    @GetMapping("/statistics")
    @PreAuthorize("hasPermission('admin', 'audit')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getActivityStatistics() {
        log.info("Activity statistics request");

        try {
            Map<String, Object> stats = userActivityService.getActivityStatistics();
            return ResponseEntity.ok(ApiResponse.success(stats));
        } catch (Exception e) {
            log.error("Error fetching activity statistics", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch activity statistics", "ACTIVITY_004"));
        }
    }

    /**
     * Clean up old activities (admin function)
     */
    @DeleteMapping("/cleanup")
    @PreAuthorize("hasPermission('admin', 'maintenance')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> cleanupOldActivities(
            @RequestParam(defaultValue = "90") int daysToKeep) {

        log.info("Cleanup old activities request - days to keep: {}", daysToKeep);

        try {
            long deletedCount = userActivityService.deleteOldActivities(daysToKeep);
            Map<String, Object> result = Map.of("deletedCount", deletedCount);
            return ResponseEntity.ok(ApiResponse.success(result, "Old activities cleaned up successfully"));
        } catch (Exception e) {
            log.error("Error cleaning up old activities", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to cleanup old activities", "ACTIVITY_005"));
        }
    }
}