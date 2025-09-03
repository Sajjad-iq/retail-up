package com.sajjadkademm.retail.domain.inventory.model;

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

import java.time.LocalDateTime;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.NotNull;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.sajjadkademm.retail.domain.organization.model.Organization;
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
@Table(name = "inventories", indexes = {
        @Index(name = "idx_inventories_id", columnList = "id"),
        @Index(name = "idx_inventories_name", columnList = "name"),
        @Index(name = "idx_inventories_organization_id", columnList = "organization_id"),
        @Index(name = "idx_inventories_name_organization", columnList = "name,organization_id"),
        @Index(name = "idx_inventories_created_at", columnList = "created_at"),
        @Index(name = "idx_inventories_is_active", columnList = "is_active"),
        @Index(name = "idx_inventories_created_by", columnList = "created_by")
})
public class Inventory {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "name", nullable = false)
    @NotBlank
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH)
    private String name;

    @Column(name = "description", length = ValidationConstants.MAX_DESCRIPTION_LENGTH)
    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH)
    private String description;

    @Column(name = "location", length = ValidationConstants.MAX_ADDRESS_LENGTH)
    @Size(max = ValidationConstants.MAX_ADDRESS_LENGTH)
    private String location;

    @Column(name = "is_active", nullable = false)
    @NotNull
    private Boolean isActive;

    @Column(name = "organization_id", nullable = false)
    @NotNull(message = "Organization ID is required")
    private String organizationId;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false)
    @JsonIgnoreProperties({ "inventories", "systemSettings" })
    private Organization organization;

    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JsonIgnoreProperties({ "password", "phone", "lastLoginAt" })
    @JoinColumn(name = "created_by", referencedColumnName = "id", nullable = false)
    private User createdBy;
}