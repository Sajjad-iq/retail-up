# Retail Auth API

A comprehensive Spring Boot authentication module built for the Retail UP application. This module provides secure JWT-based authentication, permission management, and user administration capabilities.

## 🏗️ Architecture

### Feature-Based Structure
```
src/main/java/com/sajjadkademm/retail/auth/
├── config/           # Security and application configuration
├── controllers/      # REST API endpoints
├── dto/             # Data Transfer Objects with validation
├── entities/        # JPA entities
├── exceptions/      # Custom exception classes
├── repositories/    # Spring Data JPA repositories
├── security/        # Security components (JWT, filters, etc.)
├── services/        # Business logic layer
├── utils/           # Utility classes
└── validation/      # Custom validation annotations
```

### Core Components

#### 🔐 **Authentication System**
- **JWT-based Authentication**: Stateless token-based auth
- **Cookie Support**: HTTP-only cookies for development
- **Session Management**: Multi-device session tracking
- **Security Filters**: Custom JWT authentication filter

#### 👥 **User Management**
- **Permission-Based Access Control**: Granular permissions without roles
- **User CRUD Operations**: Complete user lifecycle management
- **Activity Logging**: Comprehensive audit trail
- **Status Management**: Active, inactive, suspended, pending states

#### 🛡️ **Security Features**
- **Password Policies**: Configurable strength requirements
- **Account Lockout**: Failed login attempt protection
- **CORS Configuration**: Cross-origin request handling
- **Input Validation**: Comprehensive request validation

## 📋 Prerequisites

- **Java 21** or higher
- **Maven 3.6+**
- **PostgreSQL 12+** (optional, H2 included for development)

## 🚀 Quick Start

### 1. Clone and Setup
```bash
git clone <repository-url>
cd retail-up/retail
```

### 2. Configure Database
For **development** (H2 - default):
```properties
# Already configured in application.properties
spring.datasource.url=jdbc:h2:mem:testdb
```

For **production** (PostgreSQL):
```properties
# Uncomment in application.properties
spring.datasource.url=jdbc:postgresql://localhost:5432/retail
spring.datasource.username=postgres
spring.datasource.password=your_password
```

### 3. Run Application
```bash
mvn spring-boot:run
```

### 4. Access Endpoints
- **API Documentation**: http://localhost:8081/swagger-ui.html
- **H2 Console**: http://localhost:8081/h2-console
- **Health Check**: http://localhost:8081/api/health

## 📚 API Documentation

### 🔑 Authentication Endpoints

#### Login
```http
POST /api/auth/login
Content-Type: application/json

{
  "email": "admin@retailup.com",
  "password": "Admin123!",
  "rememberMe": false
}
```

**Response:**
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "tokenType": "Bearer",
  "expiresAt": "2024-12-20T15:30:00",
  "user": {
    "id": "user-001",
    "name": "Admin User",
    "email": "admin@retailup.com",
    "permissions": [...],
    "status": "ACTIVE"
  },
  "session": {
    "sessionId": "sess-12345",
    "createdAt": "2024-12-19T15:30:00",
    "expiresAt": "2024-12-20T15:30:00"
  }
}
```

#### Logout
```http
POST /api/auth/logout
Authorization: Bearer <token>
```

### 👤 User Management Endpoints

#### Get All Users
```http
GET /api/users
Authorization: Bearer <token>
Required Permission: users.view
```

#### Create User
```http
POST /api/users
Authorization: Bearer <token>
Required Permission: users.manage
Content-Type: application/json

{
  "name": "John Doe",
  "email": "john.doe@retailup.com",
  "password": "SecurePass123!",
  "phone": "+1-555-0123",
  "department": "Sales",
  "employeeId": "EMP001",
  "permissionIds": ["perm-pos-001", "perm-inv-001"],
  "status": "ACTIVE",
  "mustChangePassword": false
}
```

#### Update User
```http
PUT /api/users/{userId}
Authorization: Bearer <token>
Required Permission: users.manage
```

#### Delete User
```http
DELETE /api/users/{userId}
Authorization: Bearer <token>
Required Permission: users.manage
```

### 🔓 Permission Management

#### Get All Permissions
```http
GET /api/permissions
Authorization: Bearer <token>
Required Permission: users.manage or admin.full_access
```

#### Get Permissions by Category
```http
GET /api/permissions/category/{category}
Authorization: Bearer <token>
```

### 📊 Analytics Endpoints

#### User Analytics
```http
GET /api/analytics/users
Authorization: Bearer <token>
Required Permission: reports.view or admin.full_access
```

**Response:**
```json
{
  "totalUsers": 25,
  "activeUsers": 23,
  "loginsToday": 15,
  "newUsersThisMonth": 5,
  "mostActiveUsers": [...],
  "permissionDistribution": [...],
  "loginActivity": [...]
}
```

## 🔧 Configuration

### JWT Configuration
```properties
# JWT settings
jwt.secret=your-secret-key
jwt.expiration=86400000  # 24 hours in milliseconds
```

### Security Configuration
```properties
# CORS settings
security.cors.allowed-origins=http://localhost:3000,http://localhost:5173
security.cors.allowed-methods=GET,POST,PUT,DELETE,OPTIONS
security.cors.allow-credentials=true
```

### Password Policy
```properties
# Password requirements
password.min-length=8
password.require-uppercase=true
password.require-lowercase=true
password.require-digit=true
password.require-special-char=true
```

### Session Management
```properties
# Session settings
session.max-sessions=5
session.cleanup-interval=3600000
```

## 🛡️ Security Features

### Permission System
The auth module uses a **permission-based** access control system without roles:

#### Available Permissions:
- **POS**: `pos.view`, `pos.sell`, `pos.refund`, `pos.discount`
- **Inventory**: `inventory.view`, `inventory.manage`, `inventory.adjust`
- **Users**: `users.view`, `users.manage`
- **Reports**: `reports.view`, `reports.export`
- **Settings**: `settings.view`, `settings.manage`
- **Admin**: `admin.full_access`

### Authentication Flow
1. **Login**: User provides email/password
2. **Validation**: Credentials validated against database
3. **Token Generation**: JWT token created with user permissions
4. **Cookie Setting**: Token stored in HTTP-only cookie (development)
5. **Session Tracking**: Session recorded for monitoring

### Security Headers
- **Authorization**: `Bearer <token>`
- **Cookie**: `auth_token=<token>` (HTTP-only, secure in production)

## 🗄️ Database Schema

### Core Tables
- **users**: User account information
- **permissions**: System permissions
- **user_permissions**: Many-to-many relationship
- **user_activities**: Audit log
- **auth_sessions**: Session management

### Sample Data
The application initializes with:
- **Admin User**: admin@retailup.com / Admin123!
- **Manager User**: john.manager@retailup.com / Manager123!
- **Cashier User**: jane.cashier@retailup.com / Cashier123!

## 🧪 Testing

### Manual Testing with cURL

#### Login Test
```bash
curl -X POST http://localhost:8081/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "admin@retailup.com",
    "password": "Admin123!"
  }'
```

#### Protected Endpoint Test
```bash
curl -X GET http://localhost:8081/api/users \
  -H "Authorization: Bearer <your-token>"
```

### API Testing Tools
- **Swagger UI**: http://localhost:8081/swagger-ui.html
- **Postman Collection**: Available in `/docs/postman/`

## 🚀 Production Deployment

### Environment Configuration
```properties
# Production settings
spring.profiles.active=prod
spring.datasource.url=jdbc:postgresql://your-db-host:5432/retail_prod
jwt.secret=your-production-secret-key
auth.cookie.secure=true
server.port=8080
```

### Security Checklist
- [ ] Change default JWT secret
- [ ] Enable HTTPS and secure cookies
- [ ] Configure production database
- [ ] Set up monitoring and logging
- [ ] Review CORS settings
- [ ] Configure rate limiting

## 🔍 Monitoring and Logging

### Endpoints
- **Health**: `/actuator/health`
- **Metrics**: `/actuator/metrics`
- **Info**: `/actuator/info`

### Log Levels
```properties
logging.level.com.sajjadkademm.retail.auth=DEBUG
logging.level.org.springframework.security=DEBUG
```

## 🤝 Integration with Frontend

### Headers Required
```javascript
// For API requests
headers: {
  'Authorization': `Bearer ${token}`,
  'Content-Type': 'application/json'
}
```

### Cookie-based Auth (Development)
```javascript
// Cookies are automatically handled by browser
// Token stored as HTTP-only cookie: auth_token
```

### Permission Checking
```javascript
// Check user permissions from JWT payload
const permissions = user.permissions.map(p => p.name);
const canManageUsers = permissions.includes('users.manage');
```

## 📝 API Response Formats

### Success Response
```json
{
  "status": "success",
  "data": { ... },
  "timestamp": "2024-12-19T15:30:00"
}
```

### Error Response
```json
{
  "status": "error",
  "message": "Validation failed",
  "errors": {
    "email": "Email is required",
    "password": "Password must be at least 8 characters"
  },
  "timestamp": "2024-12-19T15:30:00"
}
```

## 🛠️ Development

### Adding New Permissions
1. Add permission to `DataInitializer`
2. Update security configuration
3. Add to frontend permission checks

### Extending User Model
1. Update `User` entity
2. Update DTOs and validation
3. Update mappers and services

## 📖 Additional Resources

- **OpenAPI Spec**: `/v3/api-docs`
- **Frontend Integration**: Check React auth module
- **Database Migrations**: See `/sql/migrations/`

## 🆘 Troubleshooting

### Common Issues
1. **401 Unauthorized**: Check token validity and permissions
2. **CORS Errors**: Verify allowed origins in configuration
3. **Database Connection**: Check datasource configuration
4. **Token Expiration**: Implement token refresh logic

### Support
For issues and questions, please refer to the project documentation or create an issue in the repository.

---

## 📄 License

This project is part of the Retail UP application. All rights reserved.

---

**Built with ❤️ using Spring Boot, Spring Security, and modern Java practices.** 