package com.sajjadkademm.retail.settings.system.repository;

import com.sajjadkademm.retail.settings.system.entity.SystemSetting;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface SystemSettingRepository extends JpaRepository<SystemSetting, String> {

        // Find settings by organization ID (one row per organization)
        Optional<SystemSetting> findByOrganizationId(String organizationId);

        // Check if settings exist for organization
        boolean existsByOrganizationId(String organizationId);
}