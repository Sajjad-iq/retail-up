package com.sajjadkademm.retail.shared.cqrs;

/**
 * Command bus interface for dispatching commands to their handlers.
 * Provides a centralized way to execute commands in CQRS architecture.
 */
public interface CommandBus {
    
    /**
     * Execute a command and return the result
     * 
     * @param <T> The type of the command result
     * @param command The command to execute
     * @return The result of executing the command
     * @throws Exception If command execution fails
     */
    <T> T execute(Command<T> command) throws Exception;
    
    /**
     * Execute a command asynchronously
     * 
     * @param <T> The type of the command result
     * @param command The command to execute
     * @return CompletableFuture containing the result
     */
    <T> java.util.concurrent.CompletableFuture<T> executeAsync(Command<T> command);
}