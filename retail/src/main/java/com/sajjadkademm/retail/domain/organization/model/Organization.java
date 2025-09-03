package com.sajjadkademm.retail.domain.organization.model;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Enumerated;
import jakarta.persistence.EnumType;

import java.time.LocalDateTime;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "organizations", indexes = {
        @Index(name = "idx_organizations_id", columnList = "id"),
        @Index(name = "idx_organizations_name", columnList = "name"),
        @Index(name = "idx_organizations_domain", columnList = "domain"),
        @Index(name = "idx_organizations_name_domain", columnList = "name,domain"),
        @Index(name = "idx_organizations_created_at", columnList = "created_at"),
        @Index(name = "idx_organizations_status", columnList = "status"),
        @Index(name = "idx_organizations_created_by", columnList = "created_by")
})
public class Organization {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "name", nullable = false)
    @NotBlank
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_ORGANIZATION_NAME_LENGTH)
    private String name;

    @Column(name = "domain", nullable = false, unique = true)
    @NotBlank
    @Size(min = ValidationConstants.MIN_DOMAIN_LENGTH, max = ValidationConstants.MAX_DOMAIN_LENGTH)
    private String domain;

    @Column(name = "description", length = ValidationConstants.MAX_DESCRIPTION_LENGTH)
    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH)
    private String description;

    @Column(name = "address", length = ValidationConstants.MAX_ADDRESS_LENGTH)
    @Size(max = ValidationConstants.MAX_ADDRESS_LENGTH)
    private String address;

    @Column(name = "phone", nullable = true, length = ValidationConstants.MAX_PHONE_LENGTH)
    @Size(max = ValidationConstants.MAX_PHONE_LENGTH, min = ValidationConstants.MIN_PHONE_LENGTH)
    private String phone;

    @Enumerated(EnumType.STRING)
    @Builder.Default
    @Column(name = "status", nullable = false)
    private OrganizationStatus status = OrganizationStatus.ACTIVE;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JsonIgnoreProperties({ "password", "phone", "lastLoginAt" })
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}