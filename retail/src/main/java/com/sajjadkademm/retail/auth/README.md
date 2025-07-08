# 🔐 Retail Authentication Module

A comprehensive Spring Boot authentication and authorization system designed for retail applications. This module provides JWT-based authentication, role-based access control, user management, and activity tracking.

## 📁 Directory Structure

```
auth/
├── config/           # Configuration classes
├── controllers/      # REST API controllers  
├── dto/             # Data Transfer Objects
├── entities/        # JPA entities
├── exceptions/      # Custom exceptions & global handlers
├── repositories/    # Data access layer
├── security/        # Security components
├── services/        # Business logic layer
├── utils/           # Utility classes
└── validation/      # Custom validators
```

---

## 📂 Detailed Directory Guide

### 📁 `config/` - Application Configuration

**Purpose**: Contains Spring configuration classes that set up the application infrastructure.

| File | Description | Key Features |
|------|-------------|--------------|
| `SecurityConfig.java` | Main Spring Security configuration | • JWT authentication setup<br>• CORS configuration<br>• Public endpoint definitions<br>• Security filter chain |
| `OpenApiConfig.java` | Swagger/OpenAPI documentation setup | • API documentation configuration<br>• JWT security scheme for Swagger<br>• Server information |
| `DataInitializer.java` | Bootstrap sample data on startup | • Creates default admin user<br>• Sets up permissions<br>• Initializes roles |

### 📁 `controllers/` - REST API Layer

**Purpose**: HTTP request handlers that expose the authentication functionality via REST APIs.

| File | Description | Endpoints | Features |
|------|-------------|-----------|----------|
| `AuthController.java` | Authentication endpoints | `/api/auth/login`<br>`/api/auth/logout`<br>`/api/auth/refresh` | • User login/logout<br>• JWT token management<br>• Session handling |
| `UserController.java` | User management operations | `/api/users/**` | • CRUD operations<br>• User search/filtering<br>• Password management<br>• User status updates |
| `PermissionController.java` | Permission management | `/api/permissions/**` | • Permission CRUD<br>• Permission assignment<br>• Role management |
| `AnalyticsController.java` | User activity analytics | `/api/analytics/**` | • Login statistics<br>• User activity reports<br>• Security analytics |
| `HealthController.java` | Health check endpoint | `/api/health` | • Application status<br>• Simple monitoring |

### 📁 `dto/` - Data Transfer Objects

**Purpose**: Objects used for API request/response handling, providing a clean interface between the API and internal models.

| File | Description | Usage |
|------|-------------|-------|
| `LoginRequest.java` | Login form data | • Email/password input<br>• Request validation |
| `LoginResponse.java` | Login success response | • JWT token<br>• User information<br>• Session details |
| `CreateUserRequest.java` | User creation data | • User registration<br>• Validation annotations |
| `UserResponse.java` | User information output | • Public user data<br>• Excludes sensitive fields |
| `PermissionResponse.java` | Permission data output | • Permission details<br>• Role associations |

### 📁 `entities/` - Database Models

**Purpose**: JPA entities that represent the database schema and relationships.

| File | Description | Key Fields | Relationships |
|------|-------------|------------|---------------|
| `User.java` | User account information | • Email, name, password<br>• Status, department<br>• Employee ID | • Many-to-Many with Permission<br>• One-to-Many with UserActivity |
| `Permission.java` | Access permissions/roles | • Name, description<br>• Category | • Many-to-Many with User |
| `UserActivity.java` | User action logging | • Action type, timestamp<br>• IP address, details | • Many-to-One with User |
| `AuthSession.java` | Active user sessions | • Session token, device<br>• Location, expiration | • Many-to-One with User |

### 📁 `exceptions/` - Error Handling

**Purpose**: Custom exceptions and global error handling for consistent API responses.

| File | Description | Purpose |
|------|-------------|---------|
| `GlobalExceptionHandler.java` | Centralized error handling | • Catches all exceptions<br>• Returns consistent JSON responses<br>• Handles validation errors |
| `UserNotFoundException.java` | User not found error | • Thrown when user lookup fails |
| `PermissionNotFoundException.java` | Permission not found error | • Thrown when permission lookup fails |
| `AuthenticationFailedException.java` | Authentication failure | • Thrown on login failures |

### 📁 `repositories/` - Data Access Layer

**Purpose**: Spring Data JPA repositories for database operations with custom queries.

| File | Description | Key Methods |
|------|-------------|-------------|
| `UserRepository.java` | User data access | • `findByEmail()`<br>• `findByStatus()`<br>• `searchUsers()` |
| `PermissionRepository.java` | Permission data access | • `findByName()`<br>• `findByCategory()` |
| `UserActivityRepository.java` | Activity logging access | • `findTodaysActivities()`<br>• `findByUserId()`<br>• Analytics queries |
| `AuthSessionRepository.java` | Session management | • `findActiveByUserId()`<br>• `findByToken()`<br>• Session cleanup |

### 📁 `security/` - Security Components

**Purpose**: Core Spring Security components for JWT authentication and authorization.

| File | Description | Responsibility |
|------|-------------|----------------|
| `JwtAuthenticationFilter.java` | JWT token processing | • Extracts JWT from requests<br>• Validates tokens<br>• Sets security context |
| `JwtAuthenticationEntryPoint.java` | Unauthorized access handler | • Returns 401 responses<br>• JSON error formatting |
| `CustomUserDetailsService.java` | User loading for authentication | • Loads user by email<br>• Integrates with Spring Security |
| `UserPrincipal.java` | Security context user representation | • Implements UserDetails<br>• Contains user + permissions |

### 📁 `services/` - Business Logic Layer

**Purpose**: Core business logic and service operations.

| File | Description | Key Functionality |
|------|-------------|-------------------|
| `AuthService.java` | Authentication logic | • Login/logout processing<br>• Session management<br>• Password validation |
| `UserService.java` | User management logic | • User CRUD operations<br>• Password management<br>• User search/filtering |
| `PermissionService.java` | Permission management | • Permission CRUD<br>• Role assignment<br>• Access control |
| `JwtService.java` | JWT token operations | • Token generation<br>• Token validation<br>• Claims extraction |
| `UserActivityService.java` | Activity tracking | • Action logging<br>• Analytics data<br>• Audit trails |

### 📁 `utils/` - Utility Classes

**Purpose**: Helper classes and utility functions used across the module.

| File | Description | Functions |
|------|-------------|-----------|
| `AuthUtils.java` | Authentication utilities | • ID generation<br>• Password hashing<br>• Validation helpers<br>• Security utilities |

### 📁 `validation/` - Custom Validators

**Purpose**: Custom validation annotations and logic for data integrity.

| File | Description | Validates |
|------|-------------|-----------|
| `PasswordConstraint.java` | Password validation annotation | • Password strength requirements |
| `PasswordValidator.java` | Password validation logic | • Length, complexity, patterns |
| `PhoneConstraint.java` | Phone number validation annotation | • Phone number format |
| `PhoneValidator.java` | Phone validation logic | • International phone formats |

---

## 🔄 How Authentication Works

### 1. **User Registration/Creation**
```
Admin creates user → UserService → Password hashing → Database storage
```

### 2. **Login Process**
```
1. User submits credentials (AuthController)
2. AuthService validates credentials
3. JwtService generates token
4. UserActivityService logs login
5. AuthSession created
6. JWT token returned to client
```

### 3. **Request Authentication**
```
1. Client sends request with JWT
2. JwtAuthenticationFilter extracts token
3. JwtService validates token
4. UserDetailsService loads user
5. Security context populated
6. Request proceeds to controller
```

### 4. **Authorization**
```
1. Controller method requires permissions
2. Spring Security checks user authorities
3. Access granted/denied based on permissions
4. UserActivityService logs access attempts
```

### 5. **Session Management**
```
- Multiple sessions per user supported
- Session tracking via AuthSession entity
- Automatic cleanup of expired sessions
- Device and location tracking
```

---

## 🛡️ Security Features

### **JWT Token Security**
- HS256 signature algorithm
- Configurable expiration
- Claims include user ID, email, permissions
- Token validation on every request

### **Password Security**
- BCrypt hashing with salt rounds
- Password strength validation
- Password change tracking
- Failed attempt monitoring

### **Permission System**
- Role-based access control (RBAC)
- Granular permissions
- Permission categories
- Many-to-many user-permission mapping

### **Activity Tracking**
- All user actions logged
- IP address and user agent tracking
- Failed login attempt monitoring
- Security analytics and reporting

### **Session Management**
- Multi-device session support
- Session expiration handling
- Device fingerprinting
- Geographic location tracking

---

## 🚀 API Endpoints Overview

### **Authentication**
- `POST /api/auth/login` - User login
- `POST /api/auth/logout` - User logout
- `POST /api/auth/refresh` - Refresh JWT token

### **User Management**
- `GET /api/users` - List users (paginated)
- `POST /api/users` - Create new user
- `GET /api/users/{id}` - Get user details
- `PUT /api/users/{id}` - Update user
- `DELETE /api/users/{id}` - Delete user
- `POST /api/users/{id}/change-password` - Change password

### **Permissions**
- `GET /api/permissions` - List permissions
- `POST /api/permissions` - Create permission
- `PUT /api/permissions/{id}` - Update permission
- `DELETE /api/permissions/{id}` - Delete permission

### **Analytics**
- `GET /api/analytics/login-stats` - Login statistics
- `GET /api/analytics/user-activity` - User activity reports
- `GET /api/analytics/security-events` - Security events

---

## 🔧 Configuration

### **Application Properties**
```properties
# JWT Configuration
jwt.secret=your-secret-key
jwt.expiration=86400000

# Security Configuration  
security.cors.allowed-origins=http://localhost:3000
security.cors.allowed-methods=GET,POST,PUT,DELETE,OPTIONS
security.cors.allowed-headers=*

# Password Policy
password.min-length=8
password.require-uppercase=true
password.require-digit=true
```

### **Database Configuration**
- H2 (development) / PostgreSQL (production)
- JPA with Hibernate
- Automatic schema generation
- Sample data initialization

---

## 🧪 Testing & Development

### **Swagger Documentation**
- Available at `/swagger-ui.html`
- Complete API documentation
- JWT authentication support
- Interactive testing interface

### **H2 Console**
- Available at `/h2-console` (development)
- Database inspection and queries
- User: `sa`, Password: (empty)

### **Health Monitoring**
- `/api/health` endpoint
- Application status monitoring
- Ready for production monitoring tools

---

## 📦 Dependencies

### **Core Spring Boot**
- Spring Boot Starter Web
- Spring Boot Starter Security
- Spring Boot Starter Data JPA
- Spring Boot Starter Validation

### **JWT & Security**
- JJWT (JSON Web Token library)
- BCrypt password encoding
- Spring Security

### **Database**
- H2 Database (development)
- PostgreSQL Driver (production)

### **Documentation & Utilities**
- SpringDoc OpenAPI (Swagger)
- Lombok (boilerplate reduction)
- Jakarta Validation

---

## 🏗️ Architecture Principles

### **Clean Architecture**
- Clear separation of concerns
- Dependency injection
- Interface-driven design
- Testable components

### **Security Best Practices**
- JWT stateless authentication
- Password hashing and validation
- CORS configuration
- Activity logging and monitoring

### **RESTful Design**
- Standard HTTP methods
- Consistent error responses
- Proper status codes
- Resource-based URLs

### **Database Design**
- Normalized schema
- Proper indexing
- Foreign key constraints
- Audit trail support

---

This authentication module provides a production-ready foundation for retail applications with comprehensive security features, user management, and activity tracking capabilities. 