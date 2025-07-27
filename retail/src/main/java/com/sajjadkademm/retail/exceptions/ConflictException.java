package com.sajjadkademm.retail.exceptions;

/**
 * Exception for 409 Conflict responses.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class ConflictException extends RuntimeException {

    public ConflictException(String message) {
        super(message);
    }

    public ConflictException(String message, Throwable cause) {
        super(message, cause);
    }
}