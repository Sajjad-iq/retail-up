package com.sajjadkademm.retail.settings.pos.repository;

import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface POSSettingRepository extends JpaRepository<POSSetting, String> {

        // Find settings by organization ID (one row per organization)
        Optional<POSSetting> findByOrganizationId(String organizationId);

        // Check if settings exist for organization
        boolean existsByOrganizationId(String organizationId);
}