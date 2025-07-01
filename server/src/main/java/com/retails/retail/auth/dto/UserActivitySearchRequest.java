package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.UserActivity;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

/**
 * Request DTO for user activity search and filtering
 */
@Data
public class UserActivitySearchRequest {
    private List<UUID> userIds;
    private List<UserActivity.UserAction> actions;
    private String resource;
    private String ipAddress;
    private LocalDateTime performedAfter;
    private LocalDateTime performedBefore;
    private String query; // Search in details
    private Integer page = 0;
    private Integer size = 50;
    private String sortBy = "performedAt";
    private String sortDirection = "DESC";
}