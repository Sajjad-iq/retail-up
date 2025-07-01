package com.retails.retail.auth.service;

import com.retails.retail.auth.dto.UserActivitySearchRequest;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.repository.UserActivityRepository;
import com.retails.retail.auth.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.Predicate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Service for user activity management and analytics
 * Handles activity logging, search, filtering, and analytics
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class UserActivityService {

    private final UserActivityRepository userActivityRepository;
    private final UserRepository userRepository;

    /**
     * Get user activities with pagination and filtering
     */
    public Page<UserActivity> getActivities(UserActivitySearchRequest request) {
        log.info("Fetching user activities with filters");

        Specification<UserActivity> spec = buildActivitySpecification(request);
        Sort sort = Sort.by(Sort.Direction.fromString(request.getSortDirection()), request.getSortBy());
        Pageable pageable = PageRequest.of(request.getPage(), request.getSize(), sort);

        return userActivityRepository.findAll(spec, pageable);
    }

    /**
     * Get activities for specific user
     */
    public Page<UserActivity> getActivitiesByUser(UUID userId, int page, int size) {
        log.info("Fetching activities for user: {}", userId);

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "timestamp"));
        return userActivityRepository.findByUserIdOrderByTimestampDesc(userId, pageable);
    }

    /**
     * Get recent activities (last 24 hours)
     */
    public List<UserActivity> getRecentActivities(int limit) {
        log.info("Fetching {} recent activities", limit);

        LocalDateTime since = LocalDateTime.now().minusHours(24);
        Pageable pageable = PageRequest.of(0, limit, Sort.by(Sort.Direction.DESC, "timestamp"));
        return userActivityRepository.findByTimestampAfter(since, pageable).getContent();
    }

    /**
     * Get activity statistics
     */
    public Map<String, Object> getActivityStatistics() {
        log.info("Generating activity statistics");

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalActivities", userActivityRepository.count());

        LocalDateTime startOfDay = LocalDateTime.now().withHour(0).withMinute(0).withSecond(0);
        stats.put("activitiesToday", userActivityRepository.countByTimestampAfter(startOfDay));

        LocalDateTime startOfWeek = LocalDateTime.now().minusDays(7);
        stats.put("activitiesThisWeek", userActivityRepository.countByTimestampAfter(startOfWeek));

        // Activity breakdown by action type
        Map<String, Long> actionBreakdown = new HashMap<>();
        for (UserActivity.UserAction action : UserActivity.UserAction.values()) {
            long count = userActivityRepository.countByAction(action);
            actionBreakdown.put(action.name(), count);
        }
        stats.put("actionBreakdown", actionBreakdown);

        return stats;
    }

    /**
     * Delete old activities (cleanup)
     */
    @Transactional
    public long deleteOldActivities(int daysToKeep) {
        log.info("Deleting activities older than {} days", daysToKeep);

        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(daysToKeep);
        long deletedCount = userActivityRepository.deleteByTimestampBefore(cutoffDate);

        log.info("Deleted {} old activities", deletedCount);
        return deletedCount;
    }

    // Private helper methods
    private Specification<UserActivity> buildActivitySpecification(UserActivitySearchRequest request) {
        return (root, query, criteriaBuilder) -> {
            List<Predicate> predicates = new ArrayList<>();

            if (request.getUserIds() != null && !request.getUserIds().isEmpty()) {
                predicates.add(root.get("userId").in(request.getUserIds()));
            }

            if (request.getActions() != null && !request.getActions().isEmpty()) {
                predicates.add(root.get("action").in(request.getActions()));
            }

            if (request.getPerformedAfter() != null) {
                predicates
                        .add(criteriaBuilder.greaterThanOrEqualTo(root.get("timestamp"), request.getPerformedAfter()));
            }
            if (request.getPerformedBefore() != null) {
                predicates.add(criteriaBuilder.lessThanOrEqualTo(root.get("timestamp"), request.getPerformedBefore()));
            }

            if (request.getQuery() != null && !request.getQuery().trim().isEmpty()) {
                String searchTerm = "%" + request.getQuery().toLowerCase() + "%";
                predicates.add(criteriaBuilder.like(criteriaBuilder.lower(root.get("details")), searchTerm));
            }

            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };
    }
}