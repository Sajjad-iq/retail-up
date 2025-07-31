package com.sajjadkademm.retail.settings.inventory.repository;

import com.sajjadkademm.retail.settings.inventory.entity.InventorySetting;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface InventorySettingRepository extends JpaRepository<InventorySetting, String> {

        // Find settings by organization ID (one row per organization)
        Optional<InventorySetting> findByOrganizationId(String organizationId);

        // Check if settings exist for organization
        boolean existsByOrganizationId(String organizationId);
}