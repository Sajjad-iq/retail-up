package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.Permission;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * Permission DTO for API responses
 * Maps to the frontend Permission interface
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PermissionDto {

    private UUID id;
    private String name;
    private String label;
    private String description;
    private String category;
    private Boolean isSystem;

    /**
     * Convert Permission entity to DTO
     */
    public static PermissionDto fromEntity(Permission permission) {
        if (permission == null) {
            return null;
        }

        return PermissionDto.builder()
                .id(permission.getId())
                .name(permission.getName())
                .label(permission.getLabel())
                .description(permission.getDescription())
                .category(permission.getCategory().getValue())
                .isSystem(permission.getIsSystem())
                .build();
    }

    /**
     * Get display text for UI
     */
    public String getDisplayText() {
        return this.label != null ? this.label : this.name;
    }
}