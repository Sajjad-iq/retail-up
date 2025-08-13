package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.organizations.dto.OrganizationStatus;

/**
 * Centralized validation helpers for {@link Organization} domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
public final class OrganizationValidationUtils {
    private OrganizationValidationUtils() {
    }

    /**
     * Ensures the provided organization is in an operable state.
     * Throws {@link BadRequestException} if the organization is disabled, rejected,
     * suspended, or deleted.
     *
     * @param organization the organization to validate
     * @throws BadRequestException when the organization is not active
     */
    public static void assertOrganizationIsActive(Organization organization) {
        if (organization.getStatus() == OrganizationStatus.DISABLED
                || organization.getStatus() == OrganizationStatus.REJECTED
                || organization.getStatus() == OrganizationStatus.SUSPENDED
                || organization.getStatus() == OrganizationStatus.DELETED) {
            throw new BadRequestException("This Organization Disabled or Rejected or Suspended or Deleted");
        }
    }
}