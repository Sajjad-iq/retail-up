package com.retails.retail.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * DTO for authentication analytics data
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuthAnalyticsDto {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UserStatistics {
        private Long totalUsers;
        private Long activeUsers;
        private Long inactiveUsers;
        private Long pendingUsers;
        private Long lockedUsers;
        private Long usersRequiringPasswordChange;
        private Long newUsersThisMonth;
        private Long newUsersToday;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoginStatistics {
        private Long totalLogins;
        private Long successfulLogins;
        private Long failedLogins;
        private Long loginsToday;
        private Long loginsThisWeek;
        private Long loginsThisMonth;
        private Double successRate;
        private List<LoginTrendData> loginTrends;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoginTrendData {
        private LocalDateTime date;
        private Long successfulLogins;
        private Long failedLogins;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ActivityStatistics {
        private Long totalActivities;
        private Long activitiesToday;
        private Long activitiesThisWeek;
        private Long activitiesThisMonth;
        private Map<String, Long> topActions;
        private List<String> mostActiveUsers;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SecurityStatistics {
        private Long suspiciousActivities;
        private Long passwordResets;
        private Long accountLockouts;
        private List<String> topFailedLoginIps;
        private Long multipleFailedAttempts;
    }

    private UserStatistics userStatistics;
    private LoginStatistics loginStatistics;
    private ActivityStatistics activityStatistics;
    private SecurityStatistics securityStatistics;
    private LocalDateTime generatedAt;
}