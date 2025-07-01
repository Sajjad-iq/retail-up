package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.Role;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Role DTO for API responses
 * Maps to the frontend Role interface
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RoleDto {

    private UUID id;
    private String name;
    private String description;
    private String color;
    private List<PermissionDto> permissions;
    private Boolean isSystem;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    /**
     * Convert Role entity to DTO
     */
    public static RoleDto fromEntity(Role role) {
        if (role == null) {
            return null;
        }

        return RoleDto.builder()
                .id(role.getId())
                .name(role.getName())
                .description(role.getDescription())
                .color(role.getColor())
                .permissions(role.getPermissions() != null ? role.getPermissions().stream()
                        .map(PermissionDto::fromEntity)
                        .collect(Collectors.toList()) : null)
                .isSystem(role.getIsSystem())
                .createdAt(role.getCreatedAt())
                .updatedAt(role.getUpdatedAt())
                .build();
    }

    /**
     * Get permission count
     */
    public int getPermissionCount() {
        return permissions != null ? permissions.size() : 0;
    }

    /**
     * Check if role can be deleted
     */
    public boolean canBeDeleted() {
        return !isSystem;
    }
}