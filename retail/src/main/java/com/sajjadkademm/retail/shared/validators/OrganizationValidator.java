package com.sajjadkademm.retail.shared.validators;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for Organization status rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class OrganizationValidator {

    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public OrganizationValidator(LocalizedErrorService localizedErrorService) {
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Ensures the provided organization is in an operable state.
     * Throws {@link BadRequestException} if the organization is disabled, rejected,
     * suspended, or deleted.
     *
     * @param organization the organization to validate
     * @throws BadRequestException when the organization is not active
     */
    public void assertOrganizationIsActive(Organization organization) {
        if (organization.getStatus() == OrganizationStatus.DISABLED
                || organization.getStatus() == OrganizationStatus.REJECTED
                || organization.getStatus() == OrganizationStatus.SUSPENDED
                || organization.getStatus() == OrganizationStatus.DELETED) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_INACTIVE.getMessage()));
        }
    }

    /**
     * Ensures the provided organization is not disabled by the system.
     * Throws {@link BadRequestException} if the organization is rejected,
     * suspended, or deleted.
     *
     * @param organization the organization to validate
     * @throws BadRequestException when the organization is disabled by system
     */
    public void assertOrganizationIsNotDisabledBySystem(Organization organization) {
        if (organization.getStatus() == OrganizationStatus.REJECTED
                || organization.getStatus() == OrganizationStatus.SUSPENDED
                || organization.getStatus() == OrganizationStatus.DELETED) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_INACTIVE.getMessage()));
        }
    }

}
