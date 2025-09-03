package com.sajjadkademm.retail.shared.cqrs;

/**
 * Interface for query handlers in CQRS pattern.
 * Each query handler is responsible for processing a specific type of query.
 * 
 * @param <Q> The type of query this handler processes
 * @param <R> The type of result returned by the handler
 */
public interface QueryHandler<Q extends Query<R>, R> {
    
    /**
     * Handle the query and return the result
     * 
     * @param query The query to process
     * @return The result of processing the query
     * @throws Exception If query processing fails
     */
    R handle(Q query) throws Exception;
    
    /**
     * Get the type of query this handler supports
     * 
     * @return The query class this handler processes
     */
    Class<Q> getQueryType();
    
    /**
     * Indicate if this handler supports caching
     * 
     * @return true if the query results can be cached
     */
    default boolean isCacheable() {
        return false;
    }
    
    /**
     * Get cache key for this query (if cacheable)
     * 
     * @param query The query to generate cache key for
     * @return The cache key or null if not cacheable
     */
    default String getCacheKey(Q query) {
        return null;
    }
}