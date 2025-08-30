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

}