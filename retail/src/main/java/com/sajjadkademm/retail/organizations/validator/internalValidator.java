package com.sajjadkademm.retail.organizations.validator;

import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import com.sajjadkademm.retail.shared.common.validators.PhoneValidator;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class internalValidator {
    private final LocalizedErrorService localizedErrorService;
    private final PhoneValidator phoneValidator;
    private final DomainValidator domainValidator;
    private final OrganizationRepository organizationRepository;

    /**
     * Comprehensive validation for organization creation data.
     * Validates all organization fields including email for creation requests.
     */
    public void validateOrganizationCreationData(CreateOrganizationRequest request) {

        // Validate organization name
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_EMPTY.getMessage()));
        }
        // Validate organization name length
        if (request.getName().trim().length() < 2 || request.getName().trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_INVALID.getMessage()));
        }

        // Validate description
        if (request.getDescription() == null || request.getDescription().trim().length() > 500) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DESCRIPTION_INVALID.getMessage()));
        }

        // Validate address
        if (request.getAddress() == null || request.getAddress().trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ADDRESS_INVALID.getMessage()));
        }

        // Validate phone format
        phoneValidator.validatePhoneFormatAndUniqueness(request.getPhone(),
                (phone -> organizationRepository.existsByPhone(request.getPhone())));

        // Validate domain format
        domainValidator.validateDomainFormatAndUniqueness(request.getDomain(),
                (domain -> organizationRepository.existsByDomain(request.getDomain())));
    }

    /**
     * Comprehensive validation for organization update data.
     * Validates all organization fields excluding email for update requests.
     */
    public void validateOrganizationUpdateData(UpdateOrganizationRequest request, Organization organization) {

        // Validate phone uniqueness for updates
        if (request.getPhone() != null && !request.getPhone().equals(organization.getPhone())) {
            phoneValidator.validatePhoneFormatAndUniqueness(request.getPhone(),
                    (phone -> organizationRepository.existsByPhone(request.getPhone())));
        }

        // Validate domain uniqueness for updates
        if (request.getDomain() != null && !request.getDomain().equals(organization.getDomain())) {
            domainValidator.validateDomainFormatAndUniqueness(request.getDomain(),
                    (domain -> organizationRepository.existsByDomain(request.getDomain())));
        }

        // Validate organization name
        if (request.getName() != null && request.getName().trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_EMPTY.getMessage()));
        }

        // Validate organization name length
        if (request.getName() != null && request.getName().trim().length() < 2
                || request.getName().trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_INVALID.getMessage()));
        }

        // Validate description
        if (request.getDescription() != null && request.getDescription().trim().length() > 500) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DESCRIPTION_INVALID.getMessage()));
        }

        // Validate address
        if (request.getAddress() != null && request.getAddress().trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ADDRESS_INVALID.getMessage()));
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
