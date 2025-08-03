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

    /**
     * Handle authentication failures
     */
    @ExceptionHandler({ AuthenticationFailedException.class, BadCredentialsException.class })
    public ResponseEntity<Map<String, Object>> handleAuthenticationException(Exception ex, WebRequest request) {
        log.warn("Authentication failed: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Authentication Failed",
                "message", "Invalid email or password",
                "status", HttpStatus.UNAUTHORIZED.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(response);
    }

    /**
     * Handle generic authentication exceptions
     */
    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<Map<String, Object>> handleAuthenticationException(AuthenticationException ex,
            WebRequest request) {
        log.warn("Authentication exception: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Authentication Error",
                "message", ex.getMessage(),
                "status", HttpStatus.UNAUTHORIZED.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(response);
    }

    /**
     * Handle access denied exceptions
     */
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<Map<String, Object>> handleAccessDeniedException(AccessDeniedException ex,
            WebRequest request) {
        log.warn("Access denied: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Access Denied",
                "message", "You don't have permission to access this resource",
                "status", HttpStatus.FORBIDDEN.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(response);
    }

    /**
     * Handle user not found exceptions
     */
    @ExceptionHandler(UserNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleUserNotFoundException(UserNotFoundException ex,
            WebRequest request) {
        log.warn("User not found: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "User Not Found",
                "message", ex.getMessage(),
                "status", HttpStatus.NOT_FOUND.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
    }

    /**
     * Handle permission not found exceptions
     */
    @ExceptionHandler(PermissionNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handlePermissionNotFoundException(PermissionNotFoundException ex,
            WebRequest request) {
        log.warn("Permission not found: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Permission Not Found",
                "message", ex.getMessage(),
                "status", HttpStatus.NOT_FOUND.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
    }

    /**
     * Handle 404 Not Found exceptions
     */
    @ExceptionHandler(NotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleNotFoundException(NotFoundException ex, WebRequest request) {
        log.warn("Resource not found: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Not Found",
                "message", ex.getMessage(),
                "status", HttpStatus.NOT_FOUND.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
    }

    /**
     * Handle 401 Unauthorized exceptions
     */
    @ExceptionHandler(UnauthorizedException.class)
    public ResponseEntity<Map<String, Object>> handleUnauthorizedException(UnauthorizedException ex,
            WebRequest request) {
        log.warn("Unauthorized access: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Unauthorized",
                "message", ex.getMessage(),
                "status", HttpStatus.UNAUTHORIZED.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(response);
    }

    /**
     * Handle 403 Forbidden exceptions
     */
    @ExceptionHandler(ForbiddenException.class)
    public ResponseEntity<Map<String, Object>> handleForbiddenException(ForbiddenException ex, WebRequest request) {
        log.warn("Forbidden access: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Forbidden",
                "message", ex.getMessage(),
                "status", HttpStatus.FORBIDDEN.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(response);
    }

    /**
     * Handle 400 Bad Request exceptions
     */
    @ExceptionHandler(BadRequestException.class)
    public ResponseEntity<Map<String, Object>> handleBadRequestException(BadRequestException ex, WebRequest request) {
        log.warn("Bad request: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Bad Request",
                "message", ex.getMessage(),
                "status", HttpStatus.BAD_REQUEST.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    /**
     * Handle 409 Conflict exceptions
     */
    @ExceptionHandler(ConflictException.class)
    public ResponseEntity<Map<String, Object>> handleConflictException(ConflictException ex, WebRequest request) {
        log.warn("Conflict: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Conflict",
                "message", ex.getMessage(),
                "status", HttpStatus.CONFLICT.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.CONFLICT).body(response);
    }

    /**
     * Handle 500 Internal Server Error exceptions
     */
    @ExceptionHandler(InternalServerErrorException.class)
    public ResponseEntity<Map<String, Object>> handleInternalServerErrorException(InternalServerErrorException ex,
            WebRequest request) {
        log.error("Internal server error: {}", ex.getMessage(), ex);

        Map<String, Object> response = Map.of(
                "error", "Internal Server Error",
                "message", ex.getMessage(),
                "status", HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
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

        Map<String, Object> response = Map.of(
                "error", "Validation Failed",
                "message", "Request validation failed",
                "fieldErrors", fieldErrors,
                "status", HttpStatus.BAD_REQUEST.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
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

        Map<String, Object> response = Map.of(
                "error", "Binding Failed",
                "message", "Request binding failed",
                "fieldErrors", fieldErrors,
                "status", HttpStatus.BAD_REQUEST.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    /**
     * Handle illegal argument exceptions
     */
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<Map<String, Object>> handleIllegalArgumentException(IllegalArgumentException ex,
            WebRequest request) {
        log.warn("Illegal argument: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Invalid Request",
                "message", ex.getMessage(),
                "status", HttpStatus.BAD_REQUEST.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    /**
     * Handle illegal state exceptions
     */
    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<Map<String, Object>> handleIllegalStateException(IllegalStateException ex,
            WebRequest request) {
        log.warn("Illegal state: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Invalid State",
                "message", ex.getMessage(),
                "status", HttpStatus.CONFLICT.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.CONFLICT).body(response);
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

            Map<String, Object> response = new HashMap<>();
            response.put("error", "Validation Failed");
            response.put("message", "Request validation failed due to database constraints");
            response.put("fieldErrors", fieldErrors);
            response.put("status", HttpStatus.BAD_REQUEST.value());
            response.put("timestamp", LocalDateTime.now().toString());
            response.put("path", request.getDescription(false));

            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }

        // If it's not a ConstraintViolationException, handle as generic rollback
        Map<String, Object> response = Map.of(
                "error", "Transaction Failed",
                "message", "Database transaction failed",
                "status", HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
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

        Map<String, Object> response = new HashMap<>();
        response.put("error", "Validation Failed");
        response.put("message", "Request validation failed due to database constraints");
        response.put("fieldErrors", fieldErrors);
        response.put("status", HttpStatus.BAD_REQUEST.value());
        response.put("timestamp", LocalDateTime.now().toString());
        response.put("path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

    /**
     * Handle generic runtime exceptions
     */
    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<Map<String, Object>> handleRuntimeException(RuntimeException ex, WebRequest request) {
        log.error("Runtime exception: {}", ex.getMessage(), ex);

        Map<String, Object> response = Map.of(
                "error", "Internal Server Error",
                "message", "An unexpected error occurred",
                "status", HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
    }

    /**
     * Handle generic exceptions
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<Map<String, Object>> handleGenericException(Exception ex, WebRequest request) {
        log.error("Unexpected exception: {}", ex.getMessage(), ex);

        Map<String, Object> response = Map.of(
                "error", "Internal Server Error",
                "message", "An unexpected error occurred",
                "status", HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
    }

    /**
     * Handle HttpMessageNotReadableException for missing request body errors
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<Map<String, Object>> handleHttpMessageNotReadableException(HttpMessageNotReadableException ex,
            WebRequest request) {
        log.warn("Missing request body: {}", ex.getMessage());

        Map<String, Object> response = Map.of(
                "error", "Bad Request",
                "message", "Request body is missing or invalid",
                "status", HttpStatus.BAD_REQUEST.value(),
                "timestamp", LocalDateTime.now().toString(),
                "path", request.getDescription(false));

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
    }

}