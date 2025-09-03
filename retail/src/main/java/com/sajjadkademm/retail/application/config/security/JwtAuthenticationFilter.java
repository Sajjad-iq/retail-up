package com.sajjadkademm.retail.application.config.security;

import com.sajjadkademm.retail.application.config.security.JwtUtil;
import com.sajjadkademm.retail.domain.auth.model.User;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Slf4j
@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtUtil jwtUtil;

    @Override
    protected void doFilterInternal(HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {

        try {
            final String authHeader = request.getHeader("Authorization");

            if (authHeader == null || !authHeader.startsWith("Bearer ")) {
                filterChain.doFilter(request, response);
                return;
            }

            final String jwt = authHeader.substring(7);

            // ✅ Extract all user data from JWT claims (no DB call)
            final String userId = jwtUtil.extractUserId(jwt);
            final String userName = jwtUtil.extractName(jwt);
            final String userPhone = jwtUtil.extractPhone(jwt);
            final String userEmail = jwtUtil.extractEmail(jwt);

            if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                // ✅ Create user object from JWT claims
                User user = User.builder()
                        .id(userId)
                        .name(userName)
                        .phone(userPhone)
                        .email(userEmail)
                        .build();

                // ✅ Only validate token, not user existence
                if (jwtUtil.validateToken(jwt)) {
                    UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                            user, null, null);

                    authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                    SecurityContextHolder.getContext().setAuthentication(authToken);

                    log.debug("JWT authentication successful for user: {} (from claims)", userId);
                }
            }
        } catch (Exception e) {
            log.warn("Error processing JWT token: {}", e.getMessage());
        }

        filterChain.doFilter(request, response);
    }
}