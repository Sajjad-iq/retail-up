package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.users.dto.UserStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayNameGeneration;
import org.junit.jupiter.api.DisplayNameGenerator;
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
@DisplayNameGeneration(DisplayNameGenerator.ReplaceUnderscores.class)
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
    void createUser_NullName_AllowsSave() {
        User newUser = User.builder()
                .name(null)
                .phone("111")
                .password("p")
                .status(UserStatus.ACTIVE)
                .build();
        when(userRepository.save(any(User.class))).thenReturn(testUser);
        User result = userService.createUser(newUser);
        assertNotNull(result);
        verify(userRepository).save(newUser);
    }

    @Test
    void getUserById_WhitespaceId_NotFound() {
        when(userRepository.findById(" ")).thenReturn(Optional.empty());
        assertThrows(NotFoundException.class, () -> userService.getUserById(" "));
    }

    @Test
    void updateUser_SetsAllFields() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenAnswer(inv -> inv.getArgument(0));

        User details = User.builder()
                .name("Name2")
                .phone("222")
                .password("pwd2")
                .status(UserStatus.INACTIVE)
                .build();

        User saved = userService.updateUser("user-123", details);
        assertEquals("Name2", saved.getName());
        assertEquals("222", saved.getPhone());
        assertEquals("pwd2", saved.getPassword());
        assertEquals(UserStatus.INACTIVE, saved.getStatus());
    }

    @Test
    void updateUser_DoesNotCallSave_WhenMissing() {
        when(userRepository.findById("x")).thenReturn(Optional.empty());
        assertThrows(NotFoundException.class, () -> userService.updateUser("x", updatedUser));
        verify(userRepository, never()).save(any());
    }

    @Test
    void deleteUser_PropagatesRepositoryDelete() {
        when(userRepository.existsById("user-123")).thenReturn(true);
        userService.deleteUser("user-123");
        verify(userRepository).deleteById("user-123");
    }

    @Test
    void getAllUsers_VerifyRepositoryOnce() {
        when(userRepository.findAll()).thenReturn(Arrays.asList(testUser));
        userService.getAllUsers();
        verify(userRepository, times(1)).findAll();
    }

    @Test
    void getUserById_RepositoryCalledWithSameId() {
        when(userRepository.findById("abc")).thenReturn(Optional.of(testUser));
        userService.getUserById("abc");
        verify(userRepository).findById("abc");
    }

    @Test
    void createUser_PassesThroughToSave() {
        when(userRepository.save(any(User.class))).thenReturn(testUser);
        userService.createUser(testUser);
        verify(userRepository).save(testUser);
    }

    @Test
    void updateUser_SaveReturnedEntityIsReturned() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenReturn(updatedUser);
        User saved = userService.updateUser("user-123", updatedUser);
        assertEquals("Updated User", saved.getName());
    }

    @Test
    void deleteUser_ReturnsTrueOnSuccess() {
        when(userRepository.existsById("user-123")).thenReturn(true);
        assertTrue(userService.deleteUser("user-123"));
    }

    @Test
    void getAllUsers_MultipleEntriesIntegrity() {
        List<User> users = Arrays.asList(testUser, updatedUser, testUser);
        when(userRepository.findAll()).thenReturn(users);
        List<User> res = userService.getAllUsers();
        assertEquals(3, res.size());
        assertEquals("Updated User", res.get(1).getName());
    }

    @Test
    void updateUser_PartialChangeStillOverwrites() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenAnswer(inv -> inv.getArgument(0));
        User details = User.builder().name("OnlyName").phone("123").password("p").status(UserStatus.ACTIVE).build();
        User res = userService.updateUser("user-123", details);
        assertEquals("OnlyName", res.getName());
        assertEquals("123", res.getPhone());
    }

    @Test
    void deleteUser_VerifyExistsCheck() {
        when(userRepository.existsById("user-123")).thenReturn(true);
        userService.deleteUser("user-123");
        verify(userRepository).existsById("user-123");
    }

    @Test
    void getUserById_MissingDifferentId_NotFound() {
        when(userRepository.findById("missing")).thenReturn(Optional.empty());
        assertThrows(NotFoundException.class, () -> userService.getUserById("missing"));
    }

    @Test
    void createUser_ReturnsRepositoryResult() {
        when(userRepository.save(any(User.class))).thenReturn(updatedUser);
        User res = userService.createUser(testUser);
        assertEquals("Updated User", res.getName());
    }

    @Test
    void updateUser_VerifySaveCalled() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenReturn(updatedUser);
        userService.updateUser("user-123", updatedUser);
        verify(userRepository).save(any(User.class));
    }

    @Test
    void deleteUser_NotFound_DoesNotDelete() {
        when(userRepository.existsById("none")).thenReturn(false);
        assertThrows(NotFoundException.class, () -> userService.deleteUser("none"));
        verify(userRepository, never()).deleteById(anyString());
    }

    @Test
    void getAllUsers_ResultOrderPreserved() {
        List<User> users = Arrays.asList(testUser, updatedUser);
        when(userRepository.findAll()).thenReturn(users);
        List<User> res = userService.getAllUsers();
        assertEquals("Test User", res.get(0).getName());
        assertEquals("Updated User", res.get(1).getName());
    }

    @Test
    void updateUser_DifferentIds_StillUpdatesFields() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenAnswer(inv -> inv.getArgument(0));
        User details = User.builder().name("X").phone("Y").password("Z").status(UserStatus.ACTIVE).build();
        User res = userService.updateUser("another-id-ignored", details);
        assertEquals("X", res.getName());
        assertEquals("Y", res.getPhone());
    }

    @Test
    void getAllUsers_WhenRepositoryThrows_Propagates() {
        when(userRepository.findAll()).thenThrow(new RuntimeException("db"));
        assertThrows(RuntimeException.class, () -> userService.getAllUsers());
    }

    @Test
    void createUser_WhenRepositoryThrows_Propagates() {
        when(userRepository.save(any(User.class))).thenThrow(new RuntimeException("db"));
        assertThrows(RuntimeException.class, () -> userService.createUser(testUser));
    }

    @Test
    void updateUser_WhenRepositoryThrowsOnFind_Propagates() {
        when(userRepository.findById("user-123")).thenThrow(new RuntimeException("db"));
        assertThrows(RuntimeException.class, () -> userService.updateUser("user-123", updatedUser));
    }

    @Test
    void updateUser_WhenRepositoryThrowsOnSave_Propagates() {
        when(userRepository.findById("user-123")).thenReturn(Optional.of(testUser));
        when(userRepository.save(any(User.class))).thenThrow(new RuntimeException("db"));
        assertThrows(RuntimeException.class, () -> userService.updateUser("user-123", updatedUser));
    }

    @Test
    void deleteUser_WhenRepositoryThrowsOnExists_Propagates() {
        when(userRepository.existsById("user-123")).thenThrow(new RuntimeException("db"));
        assertThrows(RuntimeException.class, () -> userService.deleteUser("user-123"));
    }

    @Test
    void deleteUser_WhenRepositoryThrowsOnDelete_Propagates() {
        when(userRepository.existsById("user-123")).thenReturn(true);
        doThrow(new RuntimeException("db")).when(userRepository).deleteById("user-123");
        assertThrows(RuntimeException.class, () -> userService.deleteUser("user-123"));
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