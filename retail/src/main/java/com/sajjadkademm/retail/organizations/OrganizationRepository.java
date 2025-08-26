package com.sajjadkademm.retail.organizations;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface OrganizationRepository extends JpaRepository<Organization, String> {

    // Find organization by name
    Optional<Organization> findByName(String name);

    // Search organizations by name
    @Query("SELECT o FROM Organization o WHERE o.name LIKE %:searchTerm%")
    List<Organization> searchOrganizations(@Param("searchTerm") String searchTerm);

    // Find organizations by creator
    List<Organization> findByCreatedBy(com.sajjadkademm.retail.users.User createdBy);

    // Search organizations by name for a specific user
    @Query("SELECT o FROM Organization o WHERE o.name LIKE %:searchTerm% AND o.createdBy.id = :userId")
    List<Organization> searchOrganizationsByUser(@Param("searchTerm") String searchTerm,
            @Param("userId") String userId);

    // Check if domain exists
    boolean existsByDomain(String domain);

    // Check if phone exists
    boolean existsByPhone(String phone);
}