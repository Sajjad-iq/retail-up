package com.sajjadkademm.retail.domain.organization.repositories;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import com.sajjadkademm.retail.domain.user.model.User;
import java.util.List;
import java.util.Optional;

public interface OrganizationRepository extends JpaRepository<Organization, String> {

    // Find organization by name
    Optional<Organization> findByName(String name);

    // Search organizations by name
    @Query("SELECT o FROM Organization o WHERE o.name LIKE %:searchTerm%")
    List<Organization> searchOrganizations(@Param("searchTerm") String searchTerm);

    // Find organizations by creator
    List<Organization> findByCreatedBy(User createdBy);

    // Search organizations by name for a specific user
    @Query("SELECT o FROM Organization o WHERE o.name LIKE %:searchTerm% AND o.createdBy.id = :userId")
    List<Organization> searchOrganizationsByUser(@Param("searchTerm") String searchTerm,
            @Param("userId") String userId);

    // Check if organization name exists
    boolean existsByName(String name);

    // Check if domain exists
    boolean existsByDomain(String domain);

    // Check if phone exists
    boolean existsByPhone(String phone);

    // Find organizations by creator ID
    List<Organization> findByCreatedById(String createdById);

    // Search organizations by user ID and search term
    @Query("SELECT o FROM Organization o WHERE (o.name LIKE %:searchTerm% OR o.description LIKE %:searchTerm%) AND o.createdBy.id = :userId")
    List<Organization> searchByUserIdAndTerm(@Param("userId") String userId, @Param("searchTerm") String searchTerm);


}