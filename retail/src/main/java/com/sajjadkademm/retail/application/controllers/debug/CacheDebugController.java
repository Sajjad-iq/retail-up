package com.sajjadkademm.retail.application.controllers.debug;

import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Debug controller for cache operations - only available in development/staging
 */
@Slf4j
@RestController
@RequestMapping("/api/debug/cache")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "app.debug.enabled", havingValue = "true", matchIfMissing = false)
@Tag(name = "Cache Debug", description = "Debug endpoints for cache management (development only)")
public class CacheDebugController {

    private final CacheInvalidationService cacheInvalidationService;

    /**
     * Force clear all caches - for debugging cache issues
     */
    @Operation(summary = "Clear All Caches", description = "Force clear all application caches (debug only)")
    @ApiResponse(responseCode = "200", description = "All caches cleared successfully")
    @PostMapping("/clear-all")
    public String clearAllCaches() {
        log.warn("DEBUG: Force clearing all caches via debug endpoint");
        cacheInvalidationService.forceClearAllCaches();
        return "All caches have been cleared!";
    }

    /**
     * Clear query cache specifically
     */
    @Operation(summary = "Clear Query Cache", description = "Clear only the query cache (debug only)")
    @ApiResponse(responseCode = "200", description = "Query cache cleared successfully")
    @PostMapping("/clear-query")
    public String clearQueryCache() {
        log.warn("DEBUG: Clearing query cache via debug endpoint");
        cacheInvalidationService.invalidateQueryCaches();
        return "Query cache cleared!";
    }

    /**
     * Clear inventory-related caches for a specific organization
     */
    @Operation(summary = "Clear Inventory Caches", description = "Clear inventory caches for a specific organization (debug only)")
    @ApiResponse(responseCode = "200", description = "Inventory caches cleared successfully")
    @PostMapping("/clear-inventory/{organizationId}")
    public String clearInventoryCaches(@PathVariable String organizationId) {
        log.warn("DEBUG: Clearing inventory caches for organization: {}", organizationId);
        cacheInvalidationService.invalidateInventoryCaches("debug-clear", organizationId);
        return "Inventory caches cleared for organization: " + organizationId;
    }

    /**
     * Get cache information
     */
    @Operation(summary = "Get Cache Info", description = "Get information about current cache state (debug only)")
    @ApiResponse(responseCode = "200", description = "Cache information retrieved")
    @GetMapping("/info")
    public String getCacheInfo() {
        return cacheInvalidationService.getCacheInfo();
    }
}