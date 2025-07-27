package com.sajjadkademm.retail.exceptions;

/**
 * Exception for 404 Not Found responses.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public class NotFoundException extends RuntimeException {

    public NotFoundException(String message) {
        super(message);
    }

    public NotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}