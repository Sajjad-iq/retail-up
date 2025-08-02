package com.sajjadkademm.retail.organizations;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import java.time.LocalDateTime;

import com.sajjadkademm.retail.users.User;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Email;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
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
    @NotBlank(message = "Organization name is required")
    @Size(min = 2, max = 100, message = "Organization name must be between 2 and 100 characters")
    private String name;

    @Column(name = "domain", nullable = false, unique = true, updatable = false)
    @NotBlank(message = "Organization domain is required")
    @Size(min = 3, max = 255, message = "Domain must be between 3 and 255 characters")
    private String domain;

    @Column(name = "description", nullable = true, length = 500)
    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;

    @Column(name = "address", nullable = true, length = 255)
    @Size(max = 255, message = "Address must not exceed 255 characters")
    private String address;

    @Column(name = "phone", nullable = true, length = 20)
    @Size(max = 20, message = "Phone must not exceed 20 characters")
    private String phone;

    @Column(name = "email", nullable = true, length = 255)
    @Email(message = "Email should be valid")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    private String email;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @Column(name = "created_by", nullable = false)
    private String createdBy;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", insertable = false, updatable = false)
    private User createdByUser;
}