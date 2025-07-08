package com.sajjadkademm.retail.auth.services;

import com.sajjadkademm.retail.auth.entities.User;
import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * JWT Service for token generation, validation, and parsing.
 * Handles all JWT-related operations for authentication.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
public class JwtService {

    @Value("${jwt.secret}")
    private String jwtSecret;

    @Value("${jwt.expiration}")
    private long jwtExpirationMs;

    /**
     * Generate JWT token for user
     */
    public String generateToken(User user) {
        Map<String, Object> claims = new HashMap<>();
        claims.put("userId", user.getId());
        claims.put("email", user.getEmail());
        claims.put("name", user.getName());
        claims.put("status", user.getStatus().getValue());
        claims.put("permissions", user.getPermissions().stream()
                .map(permission -> permission.getName())
                .toList());

        return createToken(claims, user.getEmail());
    }

    /**
     * Generate token with custom expiration
     */
    public String generateToken(User user, long expirationMs) {
        Map<String, Object> claims = new HashMap<>();
        claims.put("userId", user.getId());
        claims.put("email", user.getEmail());
        claims.put("name", user.getName());
        claims.put("status", user.getStatus().getValue());
        claims.put("permissions", user.getPermissions().stream()
                .map(permission -> permission.getName())
                .toList());

        return createToken(claims, user.getEmail(), expirationMs);
    }

    /**
     * Create JWT token with claims
     */
    private String createToken(Map<String, Object> claims, String subject) {
        return createToken(claims, subject, jwtExpirationMs);
    }

    /**
     * Create JWT token with custom expiration
     */
    private String createToken(Map<String, Object> claims, String subject, long expirationMs) {
        Date now = new Date();
        Date expiration = new Date(now.getTime() + expirationMs);

        return Jwts.builder()
                .claims(claims)
                .subject(subject)
                .issuedAt(now)
                .expiration(expiration)
                .signWith(getSigningKey())
                .compact();
    }

    /**
     * Extract email from JWT token
     */
    public String extractEmail(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    /**
     * Extract user ID from JWT token
     */
    public String extractUserId(String token) {
        return extractClaim(token, claims -> claims.get("userId", String.class));
    }

    /**
     * Extract expiration date from JWT token
     */
    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    /**
     * Extract expiration as LocalDateTime
     */
    public LocalDateTime extractExpirationAsLocalDateTime(String token) {
        Date expiration = extractExpiration(token);
        return expiration.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    /**
     * Extract specific claim from JWT token
     */
    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    /**
     * Extract all claims from JWT token
     */
    private Claims extractAllClaims(String token) {
        try {
            return Jwts.parser()
                    .verifyWith(getSigningKey())
                    .build()
                    .parseSignedClaims(token)
                    .getPayload();
        } catch (JwtException e) {
            log.error("Error extracting claims from JWT token: {}", e.getMessage());
            throw new JwtException("Invalid JWT token", e);
        }
    }

    /**
     * Check if JWT token is expired
     */
    public Boolean isTokenExpired(String token) {
        try {
            return extractExpiration(token).before(new Date());
        } catch (JwtException e) {
            log.warn("Error checking token expiration: {}", e.getMessage());
            return true;
        }
    }

    /**
     * Validate JWT token against user
     */
    public Boolean validateToken(String token, User user) {
        try {
            final String email = extractEmail(token);
            return (email.equals(user.getEmail()) && !isTokenExpired(token));
        } catch (JwtException e) {
            log.warn("Token validation failed: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Validate JWT token
     */
    public Boolean validateToken(String token) {
        try {
            extractAllClaims(token);
            return !isTokenExpired(token);
        } catch (JwtException e) {
            log.warn("Token validation failed: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Get signing key for JWT
     */
    private SecretKey getSigningKey() {
        byte[] keyBytes = jwtSecret.getBytes();
        return Keys.hmacShaKeyFor(keyBytes);
    }

    /**
     * Extract permissions from token
     */
    @SuppressWarnings("unchecked")
    public java.util.List<String> extractPermissions(String token) {
        return extractClaim(token, claims -> claims.get("permissions", java.util.List.class));
    }

    /**
     * Extract user status from token
     */
    public String extractUserStatus(String token) {
        return extractClaim(token, claims -> claims.get("status", String.class));
    }

    /**
     * Extract user name from token
     */
    public String extractUserName(String token) {
        return extractClaim(token, claims -> claims.get("name", String.class));
    }

    /**
     * Get token expiration in milliseconds
     */
    public long getJwtExpirationMs() {
        return jwtExpirationMs;
    }

    /**
     * Check if token is about to expire (within 5 minutes)
     */
    public Boolean isTokenAboutToExpire(String token) {
        try {
            Date expiration = extractExpiration(token);
            Date fiveMinutesFromNow = new Date(System.currentTimeMillis() + 5 * 60 * 1000);
            return expiration.before(fiveMinutesFromNow);
        } catch (JwtException e) {
            log.warn("Error checking token expiration: {}", e.getMessage());
            return true;
        }
    }
}