package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.users.dto.UserStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UserServiceTest {

    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private UserService userService;

    private User testUser;
    private User updatedUser;

    @BeforeEach
    void setUp() {
        testUser = User.builder()
                .id("user-123")
                .name("Test User")
                .phone("1234567890")
                .password("encoded-password")
                .status(UserStatus.ACTIVE)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        updatedUser = User.builder()
                .id("user-123")
                .name("Updated User")
                .phone("9876543210")
                .password("new-encoded-password")
                .status(UserStatus.INACTIVE)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    @Test
    void getAllUsers_Success() {
        // Given
        List<User> users = Arrays.asList(testUser, updatedUser);
        when(userRepository.findAll()).thenReturn(users);

        // When
        List<User> result = userService.getAllUsers();

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("user-123", result.get(0).getId());
        assertEquals("Test User", result.get(0).getName());
        assertEquals("Updated User", result.get(1).getName());
        verify(userRepository).findAll();
    }

    @Test
    void getAllUsers_EmptyList() {
        // Given
        when(userRepository.findAll()).thenReturn(Arrays.asList());

        // When
        List<User> result = userService.getAllUsers();

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(userRepository).findAll();
    }

    @Test
    void getUserById_Success() {
        // Given
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));

        // When
        User result = userService.getUserById("user-123");

        // Then
        assertNotNull(result);
        assertEquals("user-123", result.getId());
        assertEquals("Test User", result.getName());
        assertEquals("1234567890", result.getPhone());
        assertEquals(UserStatus.ACTIVE, result.getStatus());
        verify(userRepository).findById("user-123");
    }

    @Test
    void getUserById_NotFound() {
        // Given
        when(userRepository.findById("nonexistent-id")).thenReturn(Optional.empty());

        // When & Then
        assertThrows(NotFoundException.class, () -> userService.getUserById("nonexistent-id"));
        verify(userRepository).findById("nonexistent-id");
    }

    @Test
    void createUser_Success() {
        // Given
        User newUser = User.builder()
                .name("New User")
                .phone("5555555555")
                .password("password123")
                .status(UserStatus.ACTIVE)
                .build();

        when(userRepository.save(any(User.class))).thenReturn(testUser);

        // When
        User result = userService.createUser(newUser);

        // Then
        assertNotNull(result);
        assertEquals("user-123", result.getId());
        assertEquals("Test User", result.getName());
        verify(userRepository).save(newUser);
    }

    @Test
    void updateUser_Success() {
        // Given
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenReturn(updatedUser);

        // When
        User result = userService.updateUser("user-123", updatedUser);

        // Then
        assertNotNull(result);
        assertEquals("user-123", result.getId());
        assertEquals("Updated User", result.getName());
        assertEquals("9876543210", result.getPhone());
        assertEquals(UserStatus.INACTIVE, result.getStatus());

        verify(userRepository).findById("user-123");
        verify(userRepository).save(any(User.class));
    }

    @Test
    void updateUser_NotFound() {
        // Given
        when(userRepository.findById("nonexistent-id")).thenReturn(Optional.empty());

        // When & Then
        assertThrows(NotFoundException.class, () -> userService.updateUser("nonexistent-id", updatedUser));
        verify(userRepository).findById("nonexistent-id");
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    void updateUser_VerifyFieldsUpdated() {
        // Given
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenAnswer(invocation -> {
            User savedUser = invocation.getArgument(0);
            assertEquals("Updated User", savedUser.getName());
            assertEquals("9876543210", savedUser.getPhone());
            assertEquals("new-encoded-password", savedUser.getPassword());
            assertEquals(UserStatus.INACTIVE, savedUser.getStatus());
            return savedUser;
        });

        // When
        userService.updateUser("user-123", updatedUser);

        // Then
        verify(userRepository).findById("user-123");
        verify(userRepository).save(any(User.class));
    }

    @Test
    void deleteUser_Success() {
        // Given
        when(userRepository.existsById("user-123")).thenReturn(true);
        doNothing().when(userRepository).deleteById("user-123");

        // When
        boolean result = userService.deleteUser("user-123");

        // Then
        assertTrue(result);
        verify(userRepository).existsById("user-123");
        verify(userRepository).deleteById("user-123");
    }

    @Test
    void deleteUser_NotFound() {
        // Given
        when(userRepository.existsById("nonexistent-id")).thenReturn(false);

        // When & Then
        assertThrows(NotFoundException.class, () -> userService.deleteUser("nonexistent-id"));
        verify(userRepository).existsById("nonexistent-id");
        verify(userRepository, never()).deleteById(anyString());
    }

    @Test
    void deleteUser_VerifyDeletionNotCalled() {
        // Given
        when(userRepository.existsById("nonexistent-id")).thenReturn(false);

        // When & Then
        assertThrows(NotFoundException.class, () -> userService.deleteUser("nonexistent-id"));
        verify(userRepository, never()).deleteById(anyString());
    }
}