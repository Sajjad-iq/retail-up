package com.sajjadkademm.retail.domain.organization.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.organization.commands.UpdateOrganizationCommand;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidationUtils;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
public class UpdateOrganizationCommandHandler implements CommandHandler<UpdateOrganizationCommand, Organization> {

    private final OrganizationRepository organizationRepository;
    private final OrganizationValidationUtils validationUtils;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    @Transactional
    public Organization handle(UpdateOrganizationCommand command) {
        Organization existingOrganization = organizationRepository.findById(command.getOrganizationId())
                .orElseThrow(() -> new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));

        validationUtils.validateUserAccess(existingOrganization, command.getUserId());
        validationUtils.validateUpdateRequest(command.getRequest(), existingOrganization);

        if (command.getRequest().getName() != null) {
            existingOrganization.setName(command.getRequest().getName());
        }
        if (command.getRequest().getDescription() != null) {
            existingOrganization.setDescription(command.getRequest().getDescription());
        }
        if (command.getRequest().getDomain() != null) {
            existingOrganization.setDomain(command.getRequest().getDomain());
        }
        if (command.getRequest().getPhone() != null) {
            existingOrganization.setPhone(command.getRequest().getPhone());
        }
        if (command.getRequest().getAddress() != null) {
            existingOrganization.setAddress(command.getRequest().getAddress());
        }
        if (command.getRequest().getStatus() != null) {
            existingOrganization.setStatus(command.getRequest().getStatus());
        }

        Organization updatedOrganization = organizationRepository.save(existingOrganization);
        
        // Invalidate organization-related caches
        cacheInvalidationService.invalidateOrganizationCaches(
                updatedOrganization.getId(), 
                command.getUserId()
        );
        
        return updatedOrganization;
    }

    @Override
    public Class<UpdateOrganizationCommand> getCommandType() {
        return UpdateOrganizationCommand.class;
    }
}