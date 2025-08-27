package com.sajjadkademm.retail.organizations.dto;

import java.time.LocalDateTime;

import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class OrganizationResponse {
    private String id;
    private String name;
    private String domain;
    private String description;
    private String address;
    private String phone;
    private OrganizationStatus status;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String createdBy;
}