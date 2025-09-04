package com.sajjadkademm.retail.domain.organization.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.organization.commands.CreateOrganizationCommand;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidationUtils;
import com.sajjadkademm.retail.domain.user.repositories.UserRepository;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
public class CreateOrganizationCommandHandler implements CommandHandler<CreateOrganizationCommand, Organization> {

    private final OrganizationRepository organizationRepository;
    private final UserRepository userRepository;
    private final OrganizationValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    @Transactional
    public Organization handle(CreateOrganizationCommand command) {
        validationUtils.validateCreateRequest(command.getRequest(), command.getUserId());

        var user = userRepository.findById(command.getUserId())
                .orElseThrow(() -> new BadRequestException(localizedErrorService
                        .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage())));

        Organization organization = Organization.builder()
                .name(command.getRequest().getName())
                .description(command.getRequest().getDescription())
                .domain(command.getRequest().getDomain())
                .phone(command.getRequest().getPhone())
                .address(command.getRequest().getAddress())
                .status(com.sajjadkademm.retail.shared.enums.OrganizationStatus.ACTIVE)
                .createdBy(user)
                .build();

        Organization savedOrganization = organizationRepository.save(organization);
        
        // Invalidate organization-related caches
        cacheInvalidationService.invalidateOrganizationCaches(
                savedOrganization.getId(), 
                command.getUserId()
        );
        
        return savedOrganization;
    }

    @Override
    public Class<CreateOrganizationCommand> getCommandType() {
        return CreateOrganizationCommand.class;
    }
}