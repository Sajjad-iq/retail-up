package com.sajjadkademm.retail.domain.user.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
import com.sajjadkademm.retail.domain.user.model.User;

public interface UserRepository extends JpaRepository<User, String> {
    // Find user by email for authentication
    Optional<User> findByEmail(String email);

    // Find user by phone for authentication
    Optional<User> findByPhone(String phone);

    // Check if email exists
    boolean existsByEmail(String email);

    // Check if phone exists
    boolean existsByPhone(String phone);

    // Additional query methods can be defined here
}