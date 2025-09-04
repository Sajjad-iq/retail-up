package com.sajjadkademm.retail.shared.cache;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.CacheManager;
import org.springframework.cache.Cache;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * Centralized cache invalidation service to handle cache eviction patterns
 * for CQRS command handlers. Ensures data consistency between read and write operations.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class CacheInvalidationService {

    private final CacheManager cacheManager;

    /**
     * Invalidate organization-related caches
     * 
     * @param organizationId The organization ID to invalidate
     * @param userId The user ID who owns the organization
     */
    public void invalidateOrganizationCaches(String organizationId, String userId) {
        log.debug("Invalidating organization caches for organizationId: {} and userId: {}", organizationId, userId);
        
        // Invalidate specific organization cache
        evictFromCache("organizations", organizationId);
        
        // Invalidate user organizations list cache (user-specific)
        evictFromCache("userOrganizations", userId);
        
        // Clear organization search cache (contains search results)
        clearCache("organizationSearch");
        
        log.debug("Organization caches invalidated for organizationId: {}", organizationId);
    }

    /**
     * Invalidate inventory-related caches
     * 
     * @param inventoryId The inventory ID to invalidate
     * @param organizationId The organization ID that owns the inventory
     */
    public void invalidateInventoryCaches(String inventoryId, String organizationId) {
        log.debug("Invalidating inventory caches for inventoryId: {} and organizationId: {}", inventoryId, organizationId);
        
        // AGGRESSIVE CACHE CLEARING: Clear entire query-cache for inventory operations
        // This ensures all inventory-related cached queries are invalidated
        // In production, consider more granular cache management with Redis
        clearCache("query-cache");
        
        // Also clear inventory-specific caches
        clearCache("inventory-cache");
        clearCache("report-cache");
        
        log.debug("ALL caches cleared for inventory operation - inventoryId: {}, organizationId: {}", inventoryId, organizationId);
    }

    /**
     * Invalidate inventory item-related caches
     * 
     * @param inventoryItemId The inventory item ID to invalidate
     * @param inventoryId The parent inventory ID
     * @param organizationId The organization ID that owns the inventory
     */
    public void invalidateInventoryItemCaches(String inventoryItemId, String inventoryId, String organizationId) {
        log.debug("Invalidating inventory item caches for itemId: {}, inventoryId: {}, organizationId: {}", 
                inventoryItemId, inventoryId, organizationId);
        
        // AGGRESSIVE CACHE CLEARING: Clear entire query-cache for inventory item operations
        // This ensures all inventory-item-related cached queries are invalidated
        clearCache("query-cache");
        
        // Also clear other related caches
        clearCache("inventory-cache");
        clearCache("report-cache");
        
        log.debug("ALL caches cleared for inventory item operation - itemId: {}, inventoryId: {}, organizationId: {}", 
                inventoryItemId, inventoryId, organizationId);
    }

    /**
     * Invalidate query result caches
     */
    public void invalidateQueryCaches() {
        log.debug("Invalidating general query caches");
        clearCache("query-cache");
        clearCache("report-cache");
    }
    
    /**
     * Force clear all caches immediately - use for debugging cache issues
     */
    public void forceClearAllCaches() {
        log.warn("FORCE CLEARING ALL CACHES - this is for debugging cache issues");
        
        cacheManager.getCacheNames().forEach(cacheName -> {
            try {
                Cache cache = cacheManager.getCache(cacheName);
                if (cache != null) {
                    cache.clear();
                    log.warn("Force cleared cache: {}", cacheName);
                }
            } catch (Exception e) {
                log.error("Failed to force clear cache {}: {}", cacheName, e.getMessage());
            }
        });
        
        log.warn("All caches force cleared!");
    }

    /**
     * Invalidate all caches (use with caution)
     */
    public void invalidateAllCaches() {
        log.warn("Invalidating ALL caches - this should be used sparingly");
        
        cacheManager.getCacheNames().forEach(this::clearCache);
        
        log.warn("All caches have been invalidated");
    }

    /**
     * Evict a specific key from a cache
     * 
     * @param cacheName The name of the cache
     * @param key The key to evict
     */
    private void evictFromCache(String cacheName, String key) {
        try {
            Cache cache = cacheManager.getCache(cacheName);
            if (cache != null && key != null) {
                cache.evict(key);
                log.debug("Evicted key '{}' from cache '{}'", key, cacheName);
            }
        } catch (Exception e) {
            log.error("Failed to evict key '{}' from cache '{}': {}", key, cacheName, e.getMessage());
        }
    }

    /**
     * Evict cache entries matching a pattern (supports * wildcard)
     * Since ConcurrentMapCacheManager doesn't support pattern eviction,
     * we clear the entire cache for pattern matches
     * 
     * @param cacheName The name of the cache
     * @param keyPattern The key pattern with * wildcard
     */
    private void evictCachePattern(String cacheName, String keyPattern) {
        try {
            Cache cache = cacheManager.getCache(cacheName);
            if (cache != null && keyPattern != null) {
                // For ConcurrentMapCacheManager, we need to clear the entire cache
                // since it doesn't support pattern-based eviction
                // In production, consider using Redis with pattern support
                if (keyPattern.contains("*")) {
                    log.debug("Pattern eviction '{}' in cache '{}' - clearing entire cache", keyPattern, cacheName);
                    cache.clear();
                } else {
                    // If no wildcard, treat as exact key
                    cache.evict(keyPattern);
                    log.debug("Evicted exact key '{}' from cache '{}'", keyPattern, cacheName);
                }
            }
        } catch (Exception e) {
            log.error("Failed to evict pattern '{}' from cache '{}': {}", keyPattern, cacheName, e.getMessage());
        }
    }

    /**
     * Clear an entire cache
     * 
     * @param cacheName The name of the cache to clear
     */
    private void clearCache(String cacheName) {
        try {
            Cache cache = cacheManager.getCache(cacheName);
            if (cache != null) {
                cache.clear();
                log.debug("Cleared cache '{}'", cacheName);
            }
        } catch (Exception e) {
            log.error("Failed to clear cache '{}': {}", cacheName, e.getMessage());
        }
    }

    /**
     * Get cache statistics for monitoring
     * 
     * @return Cache information for debugging
     */
    public String getCacheInfo() {
        StringBuilder info = new StringBuilder("Cache Status:\n");
        
        cacheManager.getCacheNames().forEach(cacheName -> {
            Cache cache = cacheManager.getCache(cacheName);
            if (cache != null) {
                info.append(String.format("- Cache '%s': %s\n", 
                        cacheName, cache.getClass().getSimpleName()));
            }
        });
        
        return info.toString();
    }
}