package com.sajjadkademm.retail.exceptions;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;

import io.swagger.v3.oas.annotations.Hidden;

import org.springframework.http.converter.HttpMessageNotReadableException;
import jakarta.validation.ConstraintViolationException;
import jakarta.persistence.RollbackException;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * Global exception handler for the auth module.
 * Provides centralized exception handling for REST API endpoints.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Hidden
@RestControllerAdvice
public class GlobalExceptionHandler {

    public GlobalExceptionHandler() {
        log.info("GlobalExceptionHandler initialized and ready to handle exceptions");
    }

    private ResponseEntity<Map<String, Object>> buildErrorResponse(
            String error, String message, HttpStatus status, WebRequest request) {
        Map<String, Object> response = Map.of(
                "error", error,
                "message", message,
                "status", status.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));
        return ResponseEntity.status(status).body(response);
    }

    private ResponseEntity<Map<String, Object>> buildValidationErrorResponse(
            String error, String message, Map<String, String> fieldErrors, WebRequest request) {
        Map<String, Object> response = new HashMap<>();
        response.put("error", error);
        response.put("message", message);
        response.put("fieldErrors", fieldErrors);
        response.put("status", HttpStatus.BAD_REQUEST.value());
        response.put("timestamp", LocalDateTime.now().toString());
        response.put("path", request.getDescription(false));
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    /**
     * Handle authentication failures
     */
    @ExceptionHandler({ AuthenticationFailedException.class, BadCredentialsException.class })
    public ResponseEntity<Map<String, Object>> handleAuthenticationException(Exception ex, WebRequest request) {
        log.warn("Authentication failed: {}", ex.getMessage());
        return buildErrorResponse("Authentication Failed", "Invalid email or password", HttpStatus.UNAUTHORIZED, request);
    }

    /**
     * Handle generic authentication exceptions
     */
    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<Map<String, Object>> handleAuthenticationException(AuthenticationException ex,
            WebRequest request) {
        log.warn("Authentication exception: {}", ex.getMessage());
        return buildErrorResponse("Authentication Error", ex.getMessage(), HttpStatus.UNAUTHORIZED, request);
    }

    /**
     * Handle access denied exceptions
     */
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<Map<String, Object>> handleAccessDeniedException(AccessDeniedException ex,
            WebRequest request) {
        log.warn("Access denied: {}", ex.getMessage());
        return buildErrorResponse("Access Denied", "You don't have permission to access this resource", HttpStatus.FORBIDDEN, request);
    }

    /**
     * Handle user not found exceptions
     */
    @ExceptionHandler(UserNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleUserNotFoundException(UserNotFoundException ex,
            WebRequest request) {
        log.warn("User not found: {}", ex.getMessage());
        return buildErrorResponse("User Not Found", ex.getMessage(), HttpStatus.NOT_FOUND, request);
    }

    /**
     * Handle permission not found exceptions
     */
    @ExceptionHandler(PermissionNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handlePermissionNotFoundException(PermissionNotFoundException ex,
            WebRequest request) {
        log.warn("Permission not found: {}", ex.getMessage());
        return buildErrorResponse("Permission Not Found", ex.getMessage(), HttpStatus.NOT_FOUND, request);
    }

    /**
     * Handle 404 Not Found exceptions
     */
    @ExceptionHandler(NotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleNotFoundException(NotFoundException ex, WebRequest request) {
        log.warn("Resource not found: {}", ex.getMessage());
        return buildErrorResponse("Not Found", ex.getMessage(), HttpStatus.NOT_FOUND, request);
    }

    /**
     * Handle 401 Unauthorized exceptions
     */
    @ExceptionHandler(UnauthorizedException.class)
    public ResponseEntity<Map<String, Object>> handleUnauthorizedException(UnauthorizedException ex,
            WebRequest request) {
        log.warn("Unauthorized access: {}", ex.getMessage());
        return buildErrorResponse("Unauthorized", ex.getMessage(), HttpStatus.UNAUTHORIZED, request);
    }

    /**
     * Handle 403 Forbidden exceptions
     */
    @ExceptionHandler(ForbiddenException.class)
    public ResponseEntity<Map<String, Object>> handleForbiddenException(ForbiddenException ex, WebRequest request) {
        log.warn("Forbidden access: {}", ex.getMessage());
        return buildErrorResponse("Forbidden", ex.getMessage(), HttpStatus.FORBIDDEN, request);
    }

    /**
     * Handle 400 Bad Request exceptions
     */
    @ExceptionHandler(BadRequestException.class)
    public ResponseEntity<Map<String, Object>> handleBadRequestException(BadRequestException ex, WebRequest request) {
        log.warn("Bad request: {}", ex.getMessage());
        return buildErrorResponse("Bad Request", ex.getMessage(), HttpStatus.BAD_REQUEST, request);
    }

    /**
     * Handle 409 Conflict exceptions
     */
    @ExceptionHandler(ConflictException.class)
    public ResponseEntity<Map<String, Object>> handleConflictException(ConflictException ex, WebRequest request) {
        log.warn("Conflict: {}", ex.getMessage());
        return buildErrorResponse("Conflict", ex.getMessage(), HttpStatus.CONFLICT, request);
    }

    /**
     * Handle 500 Internal Server Error exceptions
     */
    @ExceptionHandler(InternalServerErrorException.class)
    public ResponseEntity<Map<String, Object>> handleInternalServerErrorException(InternalServerErrorException ex,
            WebRequest request) {
        log.error("Internal server error: {}", ex.getMessage(), ex);
        return buildErrorResponse("Internal Server Error", ex.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    /**
     * Handle validation errors from @Valid annotations
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, Object>> handleValidationException(MethodArgumentNotValidException ex,
            WebRequest request) {
        log.warn("Validation failed: {}", ex.getMessage());

        Map<String, String> fieldErrors = new HashMap<>();
        ex.getBindingResult().getAllErrors().forEach(error -> {
            if (error instanceof FieldError) {
                FieldError fieldError = (FieldError) error;
                fieldErrors.put(fieldError.getField(), fieldError.getDefaultMessage());
            } else {
                fieldErrors.put("global", error.getDefaultMessage());
            }
        });

        return buildValidationErrorResponse("Validation Failed", "Request validation failed", fieldErrors, request);
    }

    /**
     * Handle bind exceptions
     */
    @ExceptionHandler(BindException.class)
    public ResponseEntity<Map<String, Object>> handleBindException(BindException ex, WebRequest request) {
        log.warn("Binding failed: {}", ex.getMessage());

        Map<String, String> fieldErrors = new HashMap<>();
        ex.getBindingResult().getAllErrors().forEach(error -> {
            if (error instanceof FieldError) {
                FieldError fieldError = (FieldError) error;
                fieldErrors.put(fieldError.getField(), fieldError.getDefaultMessage());
            } else {
                fieldErrors.put("global", error.getDefaultMessage());
            }
        });

        return buildValidationErrorResponse("Binding Failed", "Request binding failed", fieldErrors, request);
    }

    /**
     * Handle illegal argument exceptions
     */
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<Map<String, Object>> handleIllegalArgumentException(IllegalArgumentException ex,
            WebRequest request) {
        log.warn("Illegal argument: {}", ex.getMessage());
        return buildErrorResponse("Invalid Request", ex.getMessage(), HttpStatus.BAD_REQUEST, request);
    }

    /**
     * Handle illegal state exceptions
     */
    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<Map<String, Object>> handleIllegalStateException(IllegalStateException ex,
            WebRequest request) {
        log.warn("Illegal state: {}", ex.getMessage());
        return buildErrorResponse("Invalid State", ex.getMessage(), HttpStatus.CONFLICT, request);
    }

    /**
     * Handle RollbackException and extract ConstraintViolationException
     */
    @ExceptionHandler(RollbackException.class)
    public ResponseEntity<Map<String, Object>> handleRollbackException(RollbackException ex, WebRequest request) {
        log.warn("Transaction rollback: {}", ex.getMessage());

        // Check if the cause is a ConstraintViolationException
        Throwable cause = ex.getCause();
        if (cause instanceof jakarta.validation.ConstraintViolationException) {
            jakarta.validation.ConstraintViolationException constraintEx = (jakarta.validation.ConstraintViolationException) cause;

            Map<String, String> fieldErrors = new HashMap<>();
            constraintEx.getConstraintViolations().forEach(violation -> {
                String fieldName = violation.getPropertyPath().toString();
                String message = violation.getMessage();
                fieldErrors.put(fieldName, message);
            });

            return buildValidationErrorResponse("Validation Failed", "Request validation failed due to database constraints", fieldErrors, request);
        }

        // If it's not a ConstraintViolationException, handle as generic rollback
        return buildErrorResponse("Transaction Failed", "Database transaction failed", HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    /**
     * Handle ConstraintViolationException for database validation errors
     */
    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<Map<String, Object>> handleConstraintViolationException(ConstraintViolationException ex,
            WebRequest request) {
        log.warn("Database validation failed: {}", ex.getMessage());

        Map<String, String> fieldErrors = new HashMap<>();
        ex.getConstraintViolations().forEach(violation -> {
            String fieldName = violation.getPropertyPath().toString();
            String message = violation.getMessage();
            fieldErrors.put(fieldName, message);
        });

        return buildValidationErrorResponse("Validation Failed", "Request validation failed due to database constraints", fieldErrors, request);
    }

    /**
     * Handle generic runtime exceptions
     */
    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<Map<String, Object>> handleRuntimeException(RuntimeException ex, WebRequest request) {
        log.error("Runtime exception: {}", ex.getMessage(), ex);
        return buildErrorResponse("Internal Server Error", "An unexpected error occurred", HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    /**
     * Handle generic exceptions
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<Map<String, Object>> handleGenericException(Exception ex, WebRequest request) {
        log.error("Unexpected exception: {}", ex.getMessage(), ex);
        return buildErrorResponse("Internal Server Error", "An unexpected error occurred", HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    /**
     * Handle HttpMessageNotReadableException for missing request body errors
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<Map<String, Object>> handleHttpMessageNotReadableException(HttpMessageNotReadableException ex,
            WebRequest request) {
        log.warn("Missing request body: {}", ex.getMessage());
        return buildErrorResponse("Bad Request", "Request body is missing or invalid", HttpStatus.BAD_REQUEST, request);
    }

}