package com.retails.retail.auth.controller;

import com.retails.retail.common.dto.ApiResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;

import java.util.function.Supplier;

/**
 * Base controller class for auth controllers
 * Contains common error handling patterns to avoid code duplication
 */
@Slf4j
public abstract class BaseAuthController {

    /**
     * Execute service operation with standard error handling
     */
    protected <T> ResponseEntity<ApiResponse<T>> executeWithErrorHandling(
            Supplier<T> serviceOperation,
            String successMessage,
            String errorPrefix,
            String logMessage) {

        try {
            T result = serviceOperation.get();
            if (successMessage != null) {
                return ResponseEntity.ok(ApiResponse.success(result, successMessage));
            } else {
                return ResponseEntity.ok(ApiResponse.success(result));
            }
        } catch (IllegalArgumentException e) {
            log.error("{}: {}", logMessage, e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), errorPrefix + "_VALIDATION"));
        } catch (Exception e) {
            log.error("{}", logMessage, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Operation failed", errorPrefix + "_ERROR"));
        }
    }

    /**
     * Execute service operation with no return value (void operations)
     */
    protected ResponseEntity<ApiResponse<String>> executeVoidWithErrorHandling(
            Runnable serviceOperation,
            String successMessage,
            String errorPrefix,
            String logMessage) {

        try {
            serviceOperation.run();
            return ResponseEntity.ok(ApiResponse.success(successMessage));
        } catch (IllegalArgumentException e) {
            log.error("{}: {}", logMessage, e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), errorPrefix + "_VALIDATION"));
        } catch (Exception e) {
            log.error("{}", logMessage, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Operation failed", errorPrefix + "_ERROR"));
        }
    }
}