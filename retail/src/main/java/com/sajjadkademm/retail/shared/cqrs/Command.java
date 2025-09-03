package com.sajjadkademm.retail.shared.cqrs;

/**
 * Marker interface for all command objects in CQRS pattern.
 * Commands represent actions that change the state of the system.
 * 
 * @param <T> The type of the command result
 */
public interface Command<T> {
    /**
     * Get a unique identifier for this command instance
     */
    default String getCommandId() {
        return this.getClass().getSimpleName() + "_" + System.currentTimeMillis();
    }
    
    /**
     * Get the user ID who initiated this command
     */
    String getUserId();
    
    /**
     * Validate the command before processing
     * @throws IllegalArgumentException if validation fails
     */
    default void validate() {
        // Default implementation - can be overridden by specific commands
    }
}