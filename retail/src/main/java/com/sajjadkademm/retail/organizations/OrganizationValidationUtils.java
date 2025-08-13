package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.organizations.dto.OrganizationStatus;

public final class OrganizationValidationUtils {
    private OrganizationValidationUtils() {
    }

    public static void assertOrganizationIsActive(Organization organization) {
        if (organization.getStatus() == OrganizationStatus.DISABLED
                || organization.getStatus() == OrganizationStatus.REJECTED
                || organization.getStatus() == OrganizationStatus.SUSPENDED
                || organization.getStatus() == OrganizationStatus.DELETED) {
            throw new BadRequestException("This Organization Disabled or Rejected or Suspended or Deleted");
        }
    }
}