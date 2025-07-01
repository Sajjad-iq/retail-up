package com.retails.retail.auth.lib;

/**
 * Constants for common repository query patterns
 * Centralizes repeated query fragments to avoid duplication
 */
public final class QueryConstants {

    private QueryConstants() {
        // Utility class - prevent instantiation
    }

    // Common Query Fragments
    public static final String NOT_DELETED_USER = "u.isDeleted = false";
    public static final String NOT_DELETED_ROLE = "r.isDeleted = false";
    public static final String NOT_DELETED_PERMISSION = "p.isDeleted = false";

    // Full Query Filters
    public static final String USER_NOT_DELETED_FILTER = " AND " + NOT_DELETED_USER;
    public static final String ROLE_NOT_DELETED_FILTER = " AND " + NOT_DELETED_ROLE;
    public static final String PERMISSION_NOT_DELETED_FILTER = " AND " + NOT_DELETED_PERMISSION;

    // Common Query Patterns
    public static final String USER_BY_EMAIL_ACTIVE = "SELECT u FROM User u WHERE LOWER(u.email) = LOWER(:email)"
            + USER_NOT_DELETED_FILTER;
    public static final String USER_BY_STATUS_ACTIVE = "SELECT u FROM User u WHERE u.status = :status"
            + USER_NOT_DELETED_FILTER;
    public static final String USER_COUNT_BY_STATUS = "SELECT COUNT(u) FROM User u WHERE u.status = :status"
            + USER_NOT_DELETED_FILTER;

    public static final String ROLE_BY_NAME_ACTIVE = "SELECT r FROM Role r WHERE LOWER(r.name) = LOWER(:name)"
            + ROLE_NOT_DELETED_FILTER;
    public static final String ROLE_ALL_ACTIVE = "SELECT r FROM Role r WHERE" + NOT_DELETED_ROLE + " ORDER BY r.name";
    public static final String ROLE_COUNT_BY_SYSTEM = "SELECT COUNT(r) FROM Role r WHERE r.isSystem = :isSystem"
            + ROLE_NOT_DELETED_FILTER;

    // Order By Clauses
    public static final String ORDER_BY_NAME = " ORDER BY r.name";
    public static final String ORDER_BY_CREATED_AT = " ORDER BY u.createdAt";
}