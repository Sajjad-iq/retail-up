package com.retails.retail.auth.dto;

import com.retails.retail.auth.entity.User;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;
import java.util.UUID;

/**
 * DTO for bulk user operations
 */
@Data
public class BulkUserOperationDto {

    public enum Operation {
        ACTIVATE,
        DEACTIVATE,
        DELETE,
        LOCK,
        UNLOCK,
        FORCE_PASSWORD_CHANGE,
        ASSIGN_ROLE,
        REMOVE_ROLE
    }

    @NotEmpty(message = "User IDs are required")
    private List<UUID> userIds;

    @NotNull(message = "Operation is required")
    private Operation operation;

    private UUID roleId; // For ASSIGN_ROLE or REMOVE_ROLE operations
    private String reason; // Optional reason for the operation
}