package com.sajjadkademm.retail.shared.cqrs.impl;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;
import org.springframework.beans.factory.annotation.Qualifier;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;

/**
 * Spring-based implementation of QueryBus.
 * Automatically discovers and registers query handlers using Spring's DI.
 * Supports caching for cacheable queries.
 */
@Slf4j
@Component
@SuppressWarnings("unchecked")
public class SpringQueryBus implements QueryBus {

    private final List<QueryHandler<?, ?>> queryHandlers;
    private final Executor taskExecutor;
    private final CacheManager cacheManager;
    
    public SpringQueryBus(List<QueryHandler<?, ?>> queryHandlers, 
                         @Qualifier("cqrsTaskExecutor") Executor taskExecutor,
                         CacheManager cacheManager) {
        this.queryHandlers = queryHandlers;
        this.taskExecutor = taskExecutor;
        this.cacheManager = cacheManager;
    }
    
    private Map<Class<?>, QueryHandler<?, ?>> handlerMap;

    @PostConstruct
    void initialize() {
        handlerMap = queryHandlers.stream()
                .collect(Collectors.toMap(
                        QueryHandler::getQueryType,
                        Function.identity()
                ));
        
        log.info("Registered {} query handlers: {}", 
                handlerMap.size(), 
                handlerMap.keySet().stream().map(Class::getSimpleName).collect(Collectors.joining(", ")));
    }

    @Override
    public <T> T execute(Query<T> query) throws Exception {
        log.debug("Executing query: {}", query.getClass().getSimpleName());
        
        // Validate query
        query.validate();
        
        // Find handler
        QueryHandler<Query<T>, T> handler = findHandler(query);
        
        // Check cache if handler supports caching
        if (handler.isCacheable()) {
            String cacheKey = handler.getCacheKey(query);
            if (cacheKey != null) {
                T cachedResult = getCachedResult(cacheKey);
                if (cachedResult != null) {
                    log.debug("Returning cached result for query: {}", query.getClass().getSimpleName());
                    return cachedResult;
                }
            }
        }
        
        // Execute query
        T result = handler.handle(query);
        
        // Cache result if applicable
        if (handler.isCacheable()) {
            String cacheKey = handler.getCacheKey(query);
            if (cacheKey != null) {
                cacheResult(cacheKey, result);
            }
        }
        
        return result;
    }

    @Override
    public <T> CompletableFuture<T> executeAsync(Query<T> query) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return execute(query);
            } catch (Exception e) {
                throw new RuntimeException("Failed to execute query: " + query.getClass().getSimpleName(), e);
            }
        }, taskExecutor);
    }

    private <T> QueryHandler<Query<T>, T> findHandler(Query<T> query) {
        QueryHandler<?, ?> handler = handlerMap.get(query.getClass());
        
        if (handler == null) {
            throw new NotFoundException("No handler found for query: " + query.getClass().getSimpleName());
        }
        
        return (QueryHandler<Query<T>, T>) handler;
    }

    @SuppressWarnings("unchecked")
    private <T> T getCachedResult(String cacheKey) {
        try {
            Cache cache = cacheManager.getCache("query-cache");
            if (cache != null) {
                Cache.ValueWrapper wrapper = cache.get(cacheKey);
                return wrapper != null ? (T) wrapper.get() : null;
            }
        } catch (Exception e) {
            log.warn("Failed to get cached result for key: {}", cacheKey, e);
        }
        return null;
    }

    private <T> void cacheResult(String cacheKey, T result) {
        try {
            Cache cache = cacheManager.getCache("query-cache");
            if (cache != null) {
                cache.put(cacheKey, result);
            }
        } catch (Exception e) {
            log.warn("Failed to cache result for key: {}", cacheKey, e);
        }
    }
}