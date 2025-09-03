package com.sajjadkademm.retail.shared.cqrs;

/**
 * Query bus interface for dispatching queries to their handlers.
 * Provides a centralized way to execute queries in CQRS architecture.
 */
public interface QueryBus {
    
    /**
     * Execute a query and return the result
     * 
     * @param <T> The type of the query result
     * @param query The query to execute
     * @return The result of executing the query
     * @throws Exception If query execution fails
     */
    <T> T execute(Query<T> query) throws Exception;
    
    /**
     * Execute a query asynchronously
     * 
     * @param <T> The type of the query result
     * @param query The query to execute
     * @return CompletableFuture containing the result
     */
    <T> java.util.concurrent.CompletableFuture<T> executeAsync(Query<T> query);
}