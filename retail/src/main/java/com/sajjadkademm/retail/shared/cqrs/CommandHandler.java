package com.sajjadkademm.retail.shared.cqrs;

/**
 * Interface for command handlers in CQRS pattern.
 * Each command handler is responsible for processing a specific type of command.
 * 
 * @param <C> The type of command this handler processes
 * @param <R> The type of result returned by the handler
 */
public interface CommandHandler<C extends Command<R>, R> {
    
    /**
     * Handle the command and return the result
     * 
     * @param command The command to process
     * @return The result of processing the command
     * @throws Exception If command processing fails
     */
    R handle(C command) throws Exception;
    
    /**
     * Get the type of command this handler supports
     * 
     * @return The command class this handler processes
     */
    Class<C> getCommandType();
    
    /**
     * Indicate if this handler requires transaction management
     * All commands are transactional by default to ensure data consistency
     * 
     * @return true - all commands require transactions
     */
    default boolean requiresTransaction() {
        return true;
    }
}