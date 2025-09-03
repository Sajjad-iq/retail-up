package com.sajjadkademm.retail.domain.inventory.repositories;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface InventoryRepository extends JpaRepository<Inventory, String> {

    // Find inventory by name
    Optional<Inventory> findByName(String name);

    // Find inventories by organization ID
    List<Inventory> findByOrganizationId(String organizationId);

    // Find active inventories by organization ID
    List<Inventory> findByOrganizationIdAndIsActiveTrue(String organizationId);

    // Search inventories by name within an organization
    @Query("SELECT i FROM Inventory i WHERE i.organizationId = :organizationId AND i.name LIKE %:searchTerm%")
    List<Inventory> searchInventoriesByOrganization(@Param("organizationId") String organizationId,
            @Param("searchTerm") String searchTerm);

    // Check if inventory name exists within an organization
    boolean existsByNameAndOrganizationId(String name, String organizationId);

    // Count inventories by organization ID
    long countByOrganizationId(String organizationId);
}