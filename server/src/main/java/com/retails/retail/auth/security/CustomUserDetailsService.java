package com.retails.retail.auth.security;

import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Custom UserDetailsService implementation
 * Loads user-specific data for Spring Security authentication
 */
@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    @Transactional(readOnly = true)
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        User user = userRepository.findByEmailIgnoreCase(email)
                .orElseThrow(() -> new UsernameNotFoundException("User not found with email: " + email));

        return UserPrincipal.create(user);
    }

    /**
     * Load user by ID (used by JWT filter)
     */
    @Transactional(readOnly = true)
    public UserDetails loadUserById(String userId) {
        User user = userRepository.findById(java.util.UUID.fromString(userId))
                .orElseThrow(() -> new UsernameNotFoundException("User not found with id: " + userId));

        return UserPrincipal.create(user);
    }

    /**
     * Custom UserPrincipal class
     */
    public static class UserPrincipal implements UserDetails {
        private final String id;
        private final String name;
        private final String email;
        private final String password;
        private final Collection<? extends GrantedAuthority> authorities;
        private final boolean enabled;
        private final boolean accountNonExpired;
        private final boolean credentialsNonExpired;
        private final boolean accountNonLocked;

        public UserPrincipal(String id, String name, String email, String password,
                Collection<? extends GrantedAuthority> authorities,
                boolean enabled, boolean accountNonExpired,
                boolean credentialsNonExpired, boolean accountNonLocked) {
            this.id = id;
            this.name = name;
            this.email = email;
            this.password = password;
            this.authorities = authorities;
            this.enabled = enabled;
            this.accountNonExpired = accountNonExpired;
            this.credentialsNonExpired = credentialsNonExpired;
            this.accountNonLocked = accountNonLocked;
        }

        public static UserPrincipal create(User user) {
            Collection<GrantedAuthority> authorities = user.getRoles().stream()
                    .flatMap(role -> role.getPermissions().stream())
                    .distinct()
                    .map(permission -> new SimpleGrantedAuthority("PERMISSION_" + permission.getName()))
                    .collect(Collectors.toList());

            // Add role authorities
            user.getRoles().forEach(
                    role -> authorities.add(new SimpleGrantedAuthority("ROLE_" + role.getName().toUpperCase())));

            return new UserPrincipal(
                    user.getId().toString(),
                    user.getName(),
                    user.getEmail(),
                    user.getPasswordHash(),
                    authorities,
                    user.isActive(),
                    true, // accountNonExpired
                    !user.getMustChangePassword(), // credentialsNonExpired
                    !User.UserStatus.SUSPENDED.equals(user.getStatus()) &&
                            !User.UserStatus.LOCKED.equals(user.getStatus()) // accountNonLocked
            );
        }

        public String getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        public String getEmail() {
            return email;
        }

        @Override
        public String getUsername() {
            return email;
        }

        @Override
        public String getPassword() {
            return password;
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return authorities;
        }

        @Override
        public boolean isAccountNonExpired() {
            return accountNonExpired;
        }

        @Override
        public boolean isAccountNonLocked() {
            return accountNonLocked;
        }

        @Override
        public boolean isCredentialsNonExpired() {
            return credentialsNonExpired;
        }

        @Override
        public boolean isEnabled() {
            return enabled;
        }
    }
}