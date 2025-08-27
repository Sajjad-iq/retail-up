package com.sajjadkademm.retail.organizations;

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
import com.sajjadkademm.retail.users.User;
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
        @Index(name = "idx_organizations_name", columnList = "name"),
        @Index(name = "idx_organizations_domain", columnList = "domain"),
        @Index(name = "idx_organizations_name_domain", columnList = "name,domain")
})
public class Organization {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "name", nullable = false)
    @NotBlank
    @Size(min = 2, max = 255)
    private String name;

    @Column(name = "domain", nullable = false, unique = true)
    @NotBlank
    @Size(min = 3, max = 255)
    private String domain;

    @Column(name = "description", length = 500)
    @Size(max = 500)
    private String description;

    @Column(name = "address", length = 255)
    @Size(max = 255)
    private String address;

    @Column(name = "phone", nullable = true, length = 20)
    @Size(max = 20, min = 8)
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