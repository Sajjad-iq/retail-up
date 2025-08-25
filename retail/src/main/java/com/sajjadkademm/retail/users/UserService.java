package com.sajjadkademm.retail.users;

import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.users.UserErrorCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;
    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public UserService(UserRepository userRepository, LocalizedErrorService localizedErrorService) {
        this.userRepository = userRepository;
        this.localizedErrorService = localizedErrorService;
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    public User getUserById(String id) {
        Optional<User> user = userRepository.findById(id);
        if (user.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }
        return user.get();
    }

    public User createUser(User user) {
        return userRepository.save(user);
    }

    public User updateUser(String id, User userDetails) {
        Optional<User> optionalUser = userRepository.findById(id);
        if (optionalUser.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }

        User user = optionalUser.get();
        user.setName(userDetails.getName());
        user.setPassword(userDetails.getPassword());
        user.setPhone(userDetails.getPhone());
        user.setStatus(userDetails.getStatus());
        return userRepository.save(user);
    }

    public boolean deleteUser(String id) {
        if (!userRepository.existsById(id)) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_FOUND.getMessage(), id));
        }
        userRepository.deleteById(id);
        return true;
    }
}
