package com.sajjadkademm.retail.organizations.validator;

import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationRepository;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.shared.validators.OrganizationStatusValidator;
import com.sajjadkademm.retail.shared.validators.PhoneValidator;
import com.sajjadkademm.retail.shared.validators.EmailValidator;
import com.sajjadkademm.retail.shared.validators.DomainValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for {@link Organization} domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class OrganizationValidationUtils {

    private final LocalizedErrorService localizedErrorService;
    private final OrganizationRepository organizationRepository;
    private final UserValidator userValidator;
    private final OrganizationStatusValidator organizationStatusValidator;
    private final PhoneValidator phoneValidator;
    private final EmailValidator emailValidator;
    private final DomainValidator domainValidator;

    @Autowired
    public OrganizationValidationUtils(LocalizedErrorService localizedErrorService,
            OrganizationRepository organizationRepository,
            UserValidator userValidator,
            OrganizationStatusValidator organizationStatusValidator,
            PhoneValidator phoneValidator,
            EmailValidator emailValidator,
            DomainValidator domainValidator) {
        this.localizedErrorService = localizedErrorService;
        this.organizationRepository = organizationRepository;
        this.userValidator = userValidator;
        this.organizationStatusValidator = organizationStatusValidator;
        this.phoneValidator = phoneValidator;
        this.emailValidator = emailValidator;
        this.domainValidator = domainValidator;
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
        organizationStatusValidator.assertOrganizationIsActive(organization);
    }

    /**
     * Ensures the provided organization is not disabled by the main system
     * due to subscription expired or other reasons.
     *
     * @param organization the organization to validate
     * @throws BadRequestException when the organization is disabled by system
     */
    public void assertOrganizationIsNotDisabledByTheSystem(Organization organization) {
        organizationStatusValidator.assertOrganizationIsNotDisabledBySystem(organization);
    }

    /**
     * Validates that the current user can create an organization.
     * Checks if user is of type USER and has ACTIVE status.
     *
     * @param currentUser the current authenticated user
     * @throws UnauthorizedException when user cannot create organization
     */
    public void validateUserCanCreateOrganization(User currentUser) {
        userValidator.assertUserIsActiveAndHasAccountType(currentUser, AccountType.USER);
    }

    /**
     * Validates that the current user can update an organization.
     * Checks if user is of type USER, has ACTIVE status, and is the creator.
     *
     * @param currentUser  the current authenticated user
     * @param organization the organization to be updated
     * @throws UnauthorizedException when user cannot update organization
     */
    public void validateUserCanUpdateOrganization(User currentUser, Organization organization) {
        User user = userValidator.validateUserActive(currentUser.getId());

        if (!user.getId().equals(organization.getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Validates that the current user can access an organization.
     * Checks if user is the creator of the organization.
     *
     * @param currentUser  the current authenticated user
     * @param organization the organization to be accessed
     * @throws UnauthorizedException when user cannot access organization
     */
    public void validateUserCanAccessOrganization(User currentUser, Organization organization) {
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            // TODO: Add admin role check here when role system is implemented
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Checks if an organization with the given phone already exists.
     * Throws ConflictException if a duplicate is found.
     *
     * @param phone the phone number to check
     * @throws ConflictException when organization with phone already exists
     */
    public void validatePhoneUniqueness(String phone) {
        phoneValidator.validatePhoneFormatAndUniqueness(phone,
                phoneNumber -> organizationRepository.existsByPhone(phoneNumber));
    }

    /**
     * Checks if an organization with the given domain already exists.
     * Throws ConflictException if a duplicate is found.
     *
     * @param domain the domain to check
     * @throws ConflictException when organization with domain already exists
     */
    public void validateDomainUniqueness(String domain) {
        domainValidator.validateDomainFormatAndUniqueness(domain,
                domainName -> organizationRepository.existsByDomain(domainName));
    }

    /**
     * Validates phone uniqueness when updating an organization.
     * Only checks if the new phone is different from the current one.
     *
     * @param newPhone     the new phone number
     * @param currentPhone the current phone number
     * @throws ConflictException when organization with new phone already exists
     */
    public void validatePhoneUniquenessForUpdate(String newPhone, String currentPhone) {
        phoneValidator.validatePhoneForUpdate(newPhone, currentPhone,
                phoneNumber -> organizationRepository.existsByPhone(phoneNumber));
    }

    /**
     * Validates domain uniqueness when updating an organization.
     * Only checks if the new domain is different from the current one.
     *
     * @param newDomain     the new domain
     * @param currentDomain the current domain
     * @throws ConflictException when organization with new domain already exists
     */
    public void validateDomainUniquenessForUpdate(String newDomain, String currentDomain) {
        domainValidator.validateDomainForUpdate(newDomain, currentDomain,
                domainName -> organizationRepository.existsByDomain(domainName));
    }

    /**
     * Validates that the search term is not empty or null.
     *
     * @param searchTerm the search term to validate
     * @throws BadRequestException when search term is empty
     */
    public void validateSearchTerm(String searchTerm) {
        if (searchTerm == null || searchTerm.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.SEARCH_TERM_EMPTY.getMessage()));
        }
    }

    /**
     * Validates organization name according to business rules.
     * Checks if name is not null, not blank, and within length constraints.
     *
     * @param name the organization name to validate
     * @throws BadRequestException when name validation fails
     */
    public void validateOrganizationName(String name) {
        if (name == null || name.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_EMPTY.getMessage()));
        }

        if (name.trim().length() < 2 || name.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_INVALID.getMessage()));
        }
    }

    /**
     * Validates organization description according to business rules.
     * Checks if description is within length constraints when provided.
     *
     * @param description the organization description to validate
     * @throws BadRequestException when description validation fails
     */
    public void validateOrganizationDescription(String description) {
        if (description != null && description.trim().length() > 500) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DESCRIPTION_INVALID.getMessage()));
        }
    }

    /**
     * Validates organization address according to business rules.
     * Checks if address is within length constraints when provided.
     *
     * @param address the organization address to validate
     * @throws BadRequestException when address validation fails
     */
    public void validateOrganizationAddress(String address) {
        if (address != null && address.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ADDRESS_INVALID.getMessage()));
        }
    }

    /**
     * Comprehensive validation for organization data.
     * Validates all organization fields according to business rules.
     *
     * @param name        the organization name
     * @param domain      the organization domain
     * @param description the organization description
     * @param address     the organization address
     * @param phone       the organization phone
     * @param email       the organization email
     * @throws BadRequestException when any validation fails
     */
    public void validateOrganizationData(String name, String domain, String description,
            String address, String phone, String email) {
        validateOrganizationName(name);
        domainValidator.validateDomainFormat(domain);
        validateOrganizationDescription(description);
        validateOrganizationAddress(address);
        phoneValidator.validatePhoneFormat(phone);
        emailValidator.validateEmailFormat(email);
    }

    /**
     * Comprehensive validation for organization creation data.
     * Validates all organization fields including email for creation requests.
     *
     * @param name        the organization name
     * @param domain      the organization domain
     * @param description the organization description
     * @param address     the organization address
     * @param phone       the organization phone
     * @param email       the organization email
     * @throws BadRequestException when any validation fails
     */
    public void validateOrganizationCreationData(String name, String domain, String description,
            String address, String phone, String email) {
        validateOrganizationName(name);
        domainValidator.validateDomainFormat(domain);
        validateOrganizationDescription(description);
        validateOrganizationAddress(address);
        phoneValidator.validatePhoneFormat(phone);
        emailValidator.validateEmailFormat(email);
    }

    /**
     * Comprehensive validation for organization update data.
     * Validates all organization fields excluding email for update requests.
     *
     * @param name        the organization name
     * @param domain      the organization domain
     * @param description the organization description
     * @param address     the organization address
     * @param phone       the organization phone
     * @throws BadRequestException when any validation fails
     */
    public void validateOrganizationUpdateData(String name, String domain, String description,
            String address, String phone) {
        validateOrganizationName(name);
        domainValidator.validateDomainFormat(domain);
        validateOrganizationDescription(description);
        validateOrganizationAddress(address);
        phoneValidator.validatePhoneFormat(phone);
    }

    /**
     * Validates that the organization status transition is allowed.
     * Checks if the new status is valid for the current organization state.
     *
     * @param currentStatus the current organization status
     * @param newStatus     the new status to validate
     * @throws BadRequestException when status transition is not allowed
     */
    public void validateStatusTransition(OrganizationStatus currentStatus, OrganizationStatus newStatus) {
        organizationStatusValidator.validateStatusTransition(currentStatus, newStatus);
    }

    /**
     * Validates that the organization can be updated.
     * Checks if the organization is in a state that allows updates.
     *
     * @param organization the organization to validate
     * @throws BadRequestException when organization cannot be updated
     */
    public void validateOrganizationCanBeUpdated(Organization organization) {
        organizationStatusValidator.assertOrganizationCanBeUpdated(organization);
    }
}