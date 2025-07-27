package com.sajjadkademm.retail.exceptions;

/**
 * Exception for 403 Forbidden responses.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class ForbiddenException extends RuntimeException {

    public ForbiddenException(String message) {
        super(message);
    }

    public ForbiddenException(String message, Throwable cause) {
        super(message, cause);
    }
}