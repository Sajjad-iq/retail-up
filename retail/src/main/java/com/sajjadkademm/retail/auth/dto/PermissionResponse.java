package com.sajjadkademm.retail.auth.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.sajjadkademm.retail.auth.entities.Permission;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Permission response DTO for returning permission data.
 * Used in API responses to provide permission details.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Permission response containing permission details")
public class PermissionResponse {

    /**
     * Permission's unique identifier
     */
    @Schema(description = "Permission's unique identifier", example = "perm-pos-001")
    private String id;

    /**
     * Permission name/code
     */
    @Schema(description = "Permission name/code", example = "pos.view")
    private String name;

    /**
     * Human-readable permission label
     */
    @Schema(description = "Human-readable permission label", example = "View POS")
    private String label;

    /**
     * Permission description
     */
    @Schema(description = "Permission description", example = "Access to view POS interface")
    private String description;

    /**
     * Permission category
     */
    @Schema(description = "Permission category", example = "POS")
    private Permission.PermissionCategory category;

    /**
     * Whether this is a system permission
     */
    @Schema(description = "Whether this is a system permission", example = "true")
    private Boolean isSystem;

    /**
     * When the permission was created
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "When the permission was created", example = "2024-01-01T10:00:00")
    private LocalDateTime createdAt;

    /**
     * When the permission was last updated
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "When the permission was last updated", example = "2024-01-15T14:30:00")
    private LocalDateTime updatedAt;
}