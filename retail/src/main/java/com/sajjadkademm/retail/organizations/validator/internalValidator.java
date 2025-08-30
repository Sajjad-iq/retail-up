package com.sajjadkademm.retail.organizations.validator;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.shared.validators.PhoneValidator;
import com.sajjadkademm.retail.shared.validators.EmailValidator;
import com.sajjadkademm.retail.shared.validators.DomainValidator;
import com.sajjadkademm.retail.organizations.OrganizationRepository;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class internalValidator {
    private final LocalizedErrorService localizedErrorService;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;
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
}
