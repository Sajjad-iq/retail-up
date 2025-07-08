package com.sajjadkademm.retail.auth.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.sajjadkademm.retail.auth.entities.User;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

/**
 * User response DTO for returning user data without sensitive information.
 * Used in API responses to provide user details.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "User response containing user details without sensitive information")
public class UserResponse {

    /**
     * User's unique identifier
     */
    @Schema(description = "User's unique identifier", example = "user-001")
    private String id;

    /**
     * User's display name
     */
    @Schema(description = "User's display name", example = "John Doe")
    private String name;

    /**
     * User's email address
     */
    @Schema(description = "User's email address", example = "john.doe@retailup.com")
    private String email;

    /**
     * User's phone number
     */
    @Schema(description = "User's phone number", example = "+1-555-0123")
    private String phone;

    /**
     * User's avatar URL
     */
    @Schema(description = "User's avatar URL", example = "https://example.com/avatar.jpg")
    private String avatar;

    /**
     * User's status
     */
    @Schema(description = "User's status", example = "ACTIVE")
    private User.UserStatus status;

    /**
     * User's department
     */
    @Schema(description = "User's department", example = "Sales")
    private String department;

    /**
     * Employee ID
     */
    @Schema(description = "Employee ID", example = "EMP001")
    private String employeeId;

    /**
     * User's permissions
     */
    @Schema(description = "User's permissions")
    private List<PermissionResponse> permissions;

    /**
     * Whether user must change password on next login
     */
    @Schema(description = "Whether user must change password on next login", example = "false")
    private Boolean mustChangePassword;

    /**
     * When the user was created
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "When the user was created", example = "2024-01-01T10:00:00")
    private LocalDateTime createdAt;

    /**
     * When the user was last updated
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "When the user was last updated", example = "2024-01-15T14:30:00")
    private LocalDateTime updatedAt;

    /**
     * Date when user last logged in
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "Date when user last logged in", example = "2024-12-19T09:15:00")
    private LocalDateTime lastLoginAt;

    /**
     * Permission count for quick access
     */
    @Schema(description = "Number of permissions assigned to user", example = "5")
    private Integer permissionCount;
}