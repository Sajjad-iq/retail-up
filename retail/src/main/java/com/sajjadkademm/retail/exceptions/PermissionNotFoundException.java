package com.sajjadkademm.retail.exceptions;

/**
 * Exception thrown when a permission is not found.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class PermissionNotFoundException extends RuntimeException {

    public PermissionNotFoundException(String message) {
        super(message);
    }

    public PermissionNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}