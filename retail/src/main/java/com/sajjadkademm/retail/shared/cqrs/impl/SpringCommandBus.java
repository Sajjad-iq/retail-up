package com.sajjadkademm.retail.shared.cqrs.impl;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
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
 * Spring-based implementation of CommandBus.
 * Automatically discovers and registers command handlers using Spring's DI.
 */
@Slf4j
@Component
@SuppressWarnings("unchecked")
public class SpringCommandBus implements CommandBus {

    private final List<CommandHandler<?, ?>> commandHandlers;
    private final Executor taskExecutor;
    
    public SpringCommandBus(List<CommandHandler<?, ?>> commandHandlers, 
                           @Qualifier("cqrsTaskExecutor") Executor taskExecutor) {
        this.commandHandlers = commandHandlers;
        this.taskExecutor = taskExecutor;
    }
    
    private Map<Class<?>, CommandHandler<?, ?>> handlerMap;

    @PostConstruct
    void initialize() {
        handlerMap = commandHandlers.stream()
                .collect(Collectors.toMap(
                        CommandHandler::getCommandType,
                        Function.identity()
                ));
        
        log.info("Registered {} command handlers: {}", 
                handlerMap.size(), 
                handlerMap.keySet().stream().map(Class::getSimpleName).collect(Collectors.joining(", ")));
    }

    @Override
    public <T> T execute(Command<T> command) throws Exception {
        log.debug("Executing command: {}", command.getClass().getSimpleName());
        
        // Validate command
        command.validate();
        
        // Find handler
        CommandHandler<Command<T>, T> handler = findHandler(command);
        
        // Execute with or without transaction based on handler configuration
        if (handler.requiresTransaction()) {
            return executeWithTransaction(command, handler);
        } else {
            return handler.handle(command);
        }
    }

    @Transactional(rollbackFor = Exception.class)
    protected <T> T executeWithTransaction(Command<T> command, CommandHandler<Command<T>, T> handler) throws Exception {
        return handler.handle(command);
    }

    @Override
    public <T> CompletableFuture<T> executeAsync(Command<T> command) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return execute(command);
            } catch (Exception e) {
                throw new RuntimeException("Failed to execute command: " + command.getClass().getSimpleName(), e);
            }
        }, taskExecutor);
    }

    private <T> CommandHandler<Command<T>, T> findHandler(Command<T> command) {
        CommandHandler<?, ?> handler = handlerMap.get(command.getClass());
        
        if (handler == null) {
            throw new NotFoundException("No handler found for command: " + command.getClass().getSimpleName());
        }
        
        return (CommandHandler<Command<T>, T>) handler;
    }
}