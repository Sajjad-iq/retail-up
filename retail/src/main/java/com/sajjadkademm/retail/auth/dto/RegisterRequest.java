package com.sajjadkademm.retail.auth.dto;

import com.sajjadkademm.retail.users.dto.UserStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RegisterRequest {
    private String name;
    private String email;
    private String phone;
    private String password;
    private UserStatus status = UserStatus.ACTIVE;
}