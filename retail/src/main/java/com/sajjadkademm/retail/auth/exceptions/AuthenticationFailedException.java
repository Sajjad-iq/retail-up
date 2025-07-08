package com.sajjadkademm.retail.auth.exceptions;

/**
 * Exception thrown when authentication fails.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class AuthenticationFailedException extends RuntimeException {

    public AuthenticationFailedException(String message) {
        super(message);
    }

    public AuthenticationFailedException(String message, Throwable cause) {
        super(message, cause);
    }
}