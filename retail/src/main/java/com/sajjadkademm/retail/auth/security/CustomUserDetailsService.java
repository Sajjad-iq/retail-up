package com.sajjadkademm.retail.auth.security;

import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.repositories.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Custom UserDetailsService implementation for Spring Security.
 * Loads user details from database for authentication.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    @Transactional(readOnly = true)
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        log.debug("Loading user by email: {}", email);

        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new UsernameNotFoundException("User not found with email: " + email));

        // Convert permissions to authorities
        List<SimpleGrantedAuthority> authorities = user.getPermissions().stream()
                .map(permission -> new SimpleGrantedAuthority(permission.getName()))
                .collect(Collectors.toList());

        // Create UserPrincipal (custom UserDetails implementation)
        return UserPrincipal.builder()
                .id(user.getId())
                .email(user.getEmail())
                .password(user.getPassword())
                .name(user.getName())
                .status(user.getStatus())
                .authorities(authorities)
                .enabled(user.getStatus() == User.UserStatus.ACTIVE)
                .accountNonExpired(true)
                .accountNonLocked(user.getStatus() != User.UserStatus.SUSPENDED)
                .credentialsNonExpired(!user.getMustChangePassword())
                .build();
    }
}