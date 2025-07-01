package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * User DTO for API responses
 * Maps to the frontend User interface
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserDto {

    private UUID id;
    private String name;
    private String email;
    private String phone;
    private String avatar;
    private List<RoleDto> roles;
    private String status;
    private String department;
    private String employeeId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime lastLoginAt;
    private Boolean mustChangePassword;

    /**
     * Convert User entity to DTO
     */
    public static UserDto fromEntity(com.retails.retail.auth.entity.User user) {
        if (user == null) {
            return null;
        }

        return UserDto.builder()
                .id(user.getId())
                .name(user.getName())
                .email(user.getEmail())
                .phone(user.getPhone())
                .avatar(user.getAvatar())
                .roles(user.getRoles().stream()
                        .map(RoleDto::fromEntity)
                        .collect(Collectors.toList()))
                .status(user.getStatus().getValue())
                .department(user.getDepartment())
                .employeeId(user.getEmployeeId())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .lastLoginAt(user.getLastLoginAt())
                .mustChangePassword(user.getMustChangePassword())
                .build();
    }

    /**
     * Get user initials for frontend
     */
    public String getInitials() {
        if (name == null || name.trim().isEmpty()) {
            return "??";
        }

        String[] parts = name.trim().split("\\s+");
        if (parts.length == 1) {
            return parts[0].substring(0, Math.min(2, parts[0].length())).toUpperCase();
        } else {
            return (parts[0].substring(0, 1) + parts[parts.length - 1].substring(0, 1)).toUpperCase();
        }
    }

    /**
     * Get display name
     */
    public String getDisplayName() {
        return this.name;
    }
}