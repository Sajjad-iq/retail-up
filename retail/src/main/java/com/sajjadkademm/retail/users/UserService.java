package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    public User getUserById(String id) {
        Optional<User> user = userRepository.findById(id);
        if (user.isEmpty()) {
            throw new NotFoundException("User not found with id: " + id);
        }
        return user.get();
    }

    public User createUser(User user) {
        return userRepository.save(user);
    }

    public User updateUser(String id, User userDetails) {
        Optional<User> optionalUser = userRepository.findById(id);
        if (optionalUser.isEmpty()) {
            throw new NotFoundException("User not found with id: " + id);
        }

        User user = optionalUser.get();
        user.setName(userDetails.getName());
        user.setEmail(userDetails.getEmail());
        user.setPassword(userDetails.getPassword());
        user.setPhone(userDetails.getPhone());
        user.setStatus(userDetails.getStatus());
        return userRepository.save(user);
    }

    public boolean deleteUser(String id) {
        if (!userRepository.existsById(id)) {
            throw new NotFoundException("User not found with id: " + id);
        }
        userRepository.deleteById(id);
        return true;
    }
}
