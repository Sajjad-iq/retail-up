package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.User;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

/**
 * Request DTO for user search and filtering
 */
@Data
public class UserSearchRequest {
    private String query; // Search in name, email, username
    private List<User.UserStatus> statuses;
    private List<UUID> roleIds;
    private LocalDateTime createdAfter;
    private LocalDateTime createdBefore;
    private LocalDateTime lastLoginAfter;
    private LocalDateTime lastLoginBefore;
    private Boolean mustChangePassword;
    private Boolean isActive;
    private Integer page = 0;
    private Integer size = 20;
    private String sortBy = "createdAt";
    private String sortDirection = "DESC";
}