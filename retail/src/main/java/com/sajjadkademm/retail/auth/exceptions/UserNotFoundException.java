package com.sajjadkademm.retail.auth.exceptions;

/**
 * Exception thrown when a user is not found.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class UserNotFoundException extends RuntimeException {

    public UserNotFoundException(String message) {
        super(message);
    }

    public UserNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}