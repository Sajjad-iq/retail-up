package com.sajjadkademm.retail.shared.validators;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class DomainValidator {

    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public DomainValidator(LocalizedErrorService localizedErrorService) {
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Validates domain format according to business rules.
     * Checks if domain is not null, not blank, and within length constraints.
     *
     * @param domain the domain to validate
     * @throws BadRequestException when domain validation fails
     */
    public void validateDomainFormat(String domain) {
        if (domain == null || domain.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.INVALID_ORGANIZATION_DATA.getMessage()));
        }

        if (domain.trim().length() < 3 || domain.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.INVALID_ORGANIZATION_DATA.getMessage()));
        }

        // Basic domain format validation (should contain at least one dot and valid
        // characters)
        if (!domain.contains(".") || domain.startsWith(".") || domain.endsWith(".")) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.INVALID_ORGANIZATION_DATA.getMessage()));
        }
    }

    /**
     * Validates domain format and checks for uniqueness.
     * Combines format validation with existence check.
     *
     * @param domain        the domain to validate
     * @param existsChecker functional interface to check if domain exists
     * @throws BadRequestException when domain validation fails
     * @throws ConflictException   when domain already exists
     */
    public void validateDomainFormatAndUniqueness(String domain, DomainExistsChecker existsChecker) {
        validateDomainFormat(domain);

        if (existsChecker.exists(domain)) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ALREADY_EXISTS.getMessage(), domain));
        }
    }

    /**
     * Validates domain uniqueness when updating.
     * Only checks if the new domain is different from the current one.
     *
     * @param newDomain     the new domain
     * @param currentDomain the current domain
     * @param existsChecker functional interface to check if domain exists
     * @throws BadRequestException when domain validation fails
     * @throws ConflictException   when domain already exists
     */
    public void validateDomainForUpdate(String newDomain, String currentDomain, DomainExistsChecker existsChecker) {
        if (newDomain != null && !newDomain.equals(currentDomain)) {
            validateDomainFormatAndUniqueness(newDomain, existsChecker);
        }
    }

    /**
     * Functional interface for checking if a domain exists.
     * This allows the validator to be decoupled from specific repository
     * implementations.
     */
    @FunctionalInterface
    public interface DomainExistsChecker {
        boolean exists(String domain);
    }
}
