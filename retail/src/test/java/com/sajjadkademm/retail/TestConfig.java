package com.sajjadkademm.retail;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import com.sajjadkademm.retail.users.UserRepository;

/**
 * Test configuration for common test beans and utilities
 */
@TestConfiguration
public class TestConfig {

    /**
     * BCryptPasswordEncoder bean for testing
     */
    @Bean
    @Primary
    public BCryptPasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    /**
     * Mock UserRepository for testing
     */
    @MockitoBean
    private UserRepository userRepository;
}