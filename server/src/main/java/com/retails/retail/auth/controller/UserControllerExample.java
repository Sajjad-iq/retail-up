package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.service.UserService;
import com.retails.retail.common.dto.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
import java.util.UUID;

/**
 * EXAMPLE: Simplified User Controller using BaseAuthController
 * This demonstrates how much duplicate code can be eliminated
 * 
 * BEFORE: ~250 lines with repetitive try-catch blocks
 * AFTER: ~50 lines with clean, focused business logic
 */
@RestController
@RequestMapping("/api/auth/users/example")
@RequiredArgsConstructor
public class UserControllerExample extends BaseAuthController {

    private final UserService userService;

    /**
     * Get all users - BEFORE: 25 lines, AFTER: 5 lines
     */
    @GetMapping
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Page<UserDto>>> getUsers(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "DESC") String sortDirection,
            @RequestParam(required = false) String query) {

        UserSearchRequest searchRequest = new UserSearchRequest();
        searchRequest.setPage(page);
        searchRequest.setSize(size);
        searchRequest.setSortBy(sortBy);
        searchRequest.setSortDirection(sortDirection);
        searchRequest.setQuery(query);

        return executeWithErrorHandling(
                () -> userService.getUsers(searchRequest),
                null,
                "USER",
                "Error fetching users");
    }

    /**
     * Create user - BEFORE: 18 lines, AFTER: 4 lines
     */
    @PostMapping
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<UserDto>> createUser(
            @Valid @RequestBody CreateUserRequest request,
            HttpServletRequest httpRequest) {

        return executeWithErrorHandling(
                () -> userService.createUser(request, httpRequest),
                "User created successfully",
                "USER",
                "Error creating user for email: " + request.getEmail());
    }

    /**
     * Activate user - BEFORE: 18 lines, AFTER: 4 lines
     */
    @PostMapping("/{userId}/activate")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> activateUser(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        return executeVoidWithErrorHandling(
                () -> userService.activateUser(userId, httpRequest),
                "User activated successfully",
                "USER",
                "Error activating user: " + userId);
    }

    /**
     * Get user statistics - BEFORE: 12 lines, AFTER: 3 lines
     */
    @GetMapping("/statistics")
    @PreAuthorize("hasPermission('users', 'view')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getUserStatistics() {
        return executeWithErrorHandling(
                userService::getUserStatistics,
                null,
                "USER",
                "Error fetching user statistics");
    }
}