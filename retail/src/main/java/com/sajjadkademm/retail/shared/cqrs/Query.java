package com.sajjadkademm.retail.shared.cqrs;

/**
 * Marker interface for all query objects in CQRS pattern.
 * Queries represent read operations that don't change system state.
 * 
 * @param <T> The type of the query result
 */
public interface Query<T> {
    /**
     * Get a unique identifier for this query instance
     */
    default String getQueryId() {
        return this.getClass().getSimpleName() + "_" + System.currentTimeMillis();
    }
    
    /**
     * Get the user ID who initiated this query (for authorization)
     */
    String getUserId();
    
    /**
     * Validate the query parameters before processing
     * @throws IllegalArgumentException if validation fails
     */
    default void validate() {
        // Default implementation - can be overridden by specific queries
    }
}