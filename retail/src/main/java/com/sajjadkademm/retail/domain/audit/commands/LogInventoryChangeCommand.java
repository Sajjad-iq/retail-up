package com.sajjadkademm.retail.domain.audit.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class LogInventoryChangeCommand implements Command<String> {
    private final String organizationId;
    private final String itemId;
    private final String itemName;
    private final String movementType;
    private final Integer quantityChange;
    private final Integer stockBefore;
    private final Integer stockAfter;
    private final String reason;
    private final String referenceType;
    private final String referenceId;
    private final User user;
    private final String userId;
    
    @Override
    public String getUserId() {
        return userId;
    }
}