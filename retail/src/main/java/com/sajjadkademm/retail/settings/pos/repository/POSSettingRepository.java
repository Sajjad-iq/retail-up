package com.sajjadkademm.retail.settings.pos.repository;

import com.sajjadkademm.retail.settings.pos.entity.POSSetting;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface POSSettingRepository extends JpaRepository<POSSetting, String> {

    // Find settings by organization ID
    List<POSSetting> findByOrganizationId(String organizationId);

    // Find settings by organization ID and setting type
    List<POSSetting> findByOrganizationIdAndSettingType(String organizationId, String settingType);

    // Find setting by organization ID and key
    Optional<POSSetting> findByOrganizationIdAndKey(String organizationId, String key);

    // Check if setting exists by organization ID and key
    boolean existsByOrganizationIdAndKey(String organizationId, String key);

    // Find default settings by organization ID
    List<POSSetting> findByOrganizationIdAndIsDefaultTrue(String organizationId);

    // Find settings by organization ID and setting type that are not default
    List<POSSetting> findByOrganizationIdAndSettingTypeAndIsDefaultFalse(String organizationId, String settingType);

    // Search settings by organization ID and key
    @Query("SELECT s FROM POSSetting s WHERE s.organizationId = :organizationId AND s.key LIKE %:searchTerm%")
    List<POSSetting> searchSettingsByKey(@Param("organizationId") String organizationId,
            @Param("searchTerm") String searchTerm);

    // Search settings by organization ID and setting type
    @Query("SELECT s FROM POSSetting s WHERE s.organizationId = :organizationId AND s.settingType LIKE %:searchTerm%")
    List<POSSetting> searchSettingsByType(@Param("organizationId") String organizationId,
            @Param("searchTerm") String searchTerm);

    // Delete settings by organization ID and setting type
    void deleteByOrganizationIdAndSettingType(String organizationId, String settingType);

    // Delete setting by organization ID and key
    void deleteByOrganizationIdAndKey(String organizationId, String key);
}