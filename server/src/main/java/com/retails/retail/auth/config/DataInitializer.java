package com.retails.retail.auth.config;

import com.retails.retail.auth.entity.Permission;
import com.retails.retail.auth.entity.Role;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.repository.PermissionRepository;
import com.retails.retail.auth.repository.RoleRepository;
import com.retails.retail.auth.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

/**
 * Data Initializer
 * Seeds the database with default permissions, roles, and admin user
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class DataInitializer implements CommandLineRunner {

        private final PermissionRepository permissionRepository;
        private final RoleRepository roleRepository;
        private final UserRepository userRepository;
        private final PasswordEncoder passwordEncoder;

        @Override
        @Transactional
        public void run(String... args) throws Exception {
                log.info("Initializing database with default data...");

                // Initialize permissions
                initializePermissions();

                // Initialize roles
                initializeRoles();

                // Initialize admin user
                initializeAdminUser();

                log.info("Database initialization completed successfully!");
        }

        private void initializePermissions() {
                log.info("Initializing permissions...");

                List<Permission> permissions = Arrays.asList(
                                // POS Permissions
                                createPermission("pos.view", "View POS", "Access to view POS interface",
                                                Permission.PermissionCategory.POS),
                                createPermission("pos.sell", "Process Sales", "Process sales transactions",
                                                Permission.PermissionCategory.POS),
                                createPermission("pos.refund", "Process Refunds", "Process refunds and returns",
                                                Permission.PermissionCategory.POS),
                                createPermission("pos.discount", "Apply Discounts", "Apply discounts to transactions",
                                                Permission.PermissionCategory.POS),
                                createPermission("pos.hold", "Hold Transactions", "Hold and resume transactions",
                                                Permission.PermissionCategory.POS),

                                // Inventory Permissions
                                createPermission("inventory.view", "View Inventory", "Access to view inventory",
                                                Permission.PermissionCategory.INVENTORY),
                                createPermission("inventory.manage", "Manage Inventory",
                                                "Add, edit, delete inventory items",
                                                Permission.PermissionCategory.INVENTORY),
                                createPermission("inventory.adjust", "Adjust Stock", "Adjust stock levels",
                                                Permission.PermissionCategory.INVENTORY),
                                createPermission("inventory.transfer", "Transfer Stock",
                                                "Transfer stock between locations",
                                                Permission.PermissionCategory.INVENTORY),

                                // User Management Permissions
                                createPermission("users.view", "View Users", "View user list and details",
                                                Permission.PermissionCategory.USERS),
                                createPermission("users.manage", "Manage Users", "Create, edit, delete users",
                                                Permission.PermissionCategory.USERS),
                                createPermission("users.roles", "Manage Roles", "Create, edit, delete roles",
                                                Permission.PermissionCategory.USERS),
                                createPermission("users.permissions", "Manage Permissions",
                                                "Assign permissions to roles",
                                                Permission.PermissionCategory.USERS),

                                // Reports Permissions
                                createPermission("reports.view", "View Reports", "Access to view reports",
                                                Permission.PermissionCategory.REPORTS),
                                createPermission("reports.export", "Export Reports", "Export reports to files",
                                                Permission.PermissionCategory.REPORTS),
                                createPermission("reports.create", "Create Reports", "Create custom reports",
                                                Permission.PermissionCategory.REPORTS),

                                // Settings Permissions
                                createPermission("settings.view", "View Settings", "Access to view system settings",
                                                Permission.PermissionCategory.SETTINGS),
                                createPermission("settings.manage", "Manage Settings", "Modify system settings",
                                                Permission.PermissionCategory.SETTINGS),
                                createPermission("settings.backup", "Backup System", "Create and restore backups",
                                                Permission.PermissionCategory.SETTINGS),

                                // Admin Permissions
                                createPermission("admin.full_access", "Full Admin Access",
                                                "Complete system administration access",
                                                Permission.PermissionCategory.ADMIN),
                                createPermission("admin.audit", "View Audit Logs", "Access to system audit logs",
                                                Permission.PermissionCategory.ADMIN),
                                createPermission("admin.maintenance", "System Maintenance",
                                                "System maintenance operations",
                                                Permission.PermissionCategory.ADMIN));

                permissions.forEach(permission -> {
                        if (!permissionRepository.existsByName(permission.getName())) {
                                permissionRepository.save(permission);
                                log.debug("Created permission: {}", permission.getName());
                        }
                });

                log.info("Permissions initialization completed");
        }

        private void initializeRoles() {
                log.info("Initializing roles...");

                List<Permission> allPermissions = permissionRepository.findAll();

                // Admin Role
                if (!roleRepository.existsByNameIgnoreCase("Admin")) {
                        Role adminRole = Role.builder()
                                        .name("Admin")
                                        .description("System administrator with full access")
                                        .color("#DC2626")
                                        .permissions(new HashSet<>(allPermissions))
                                        .isSystem(true)
                                        .isDeleted(false)
                                        .build();
                        roleRepository.save(adminRole);
                        log.info("Created Admin role");
                }

                // Manager Role
                if (!roleRepository.existsByNameIgnoreCase("Manager")) {
                        List<Permission> managerPermissions = allPermissions.stream()
                                        .filter(p -> !p.getName().startsWith("admin.")
                                                        && !p.getName().equals("users.permissions"))
                                        .toList();

                        Role managerRole = Role.builder()
                                        .name("Manager")
                                        .description("Store manager with most permissions")
                                        .color("#7C3AED")
                                        .permissions(new HashSet<>(managerPermissions))
                                        .isSystem(true)
                                        .isDeleted(false)
                                        .build();
                        roleRepository.save(managerRole);
                        log.info("Created Manager role");
                }

                // Cashier Role
                if (!roleRepository.existsByNameIgnoreCase("Cashier")) {
                        List<Permission> cashierPermissions = allPermissions.stream()
                                        .filter(p -> p.getCategory() == Permission.PermissionCategory.POS ||
                                                        (p.getCategory() == Permission.PermissionCategory.INVENTORY
                                                                        && p.getName().equals("inventory.view")))
                                        .toList();

                        Role cashierRole = Role.builder()
                                        .name("Cashier")
                                        .description("Front-of-house staff for POS operations")
                                        .color("#059669")
                                        .permissions(new HashSet<>(cashierPermissions))
                                        .isSystem(true)
                                        .isDeleted(false)
                                        .build();
                        roleRepository.save(cashierRole);
                        log.info("Created Cashier role");
                }

                // Stock Clerk Role
                if (!roleRepository.existsByNameIgnoreCase("Stock Clerk")) {
                        List<Permission> stockPermissions = allPermissions.stream()
                                        .filter(p -> p.getCategory() == Permission.PermissionCategory.INVENTORY)
                                        .toList();

                        Role stockRole = Role.builder()
                                        .name("Stock Clerk")
                                        .description("Inventory management staff")
                                        .color("#0891B2")
                                        .permissions(new HashSet<>(stockPermissions))
                                        .isSystem(true)
                                        .isDeleted(false)
                                        .build();
                        roleRepository.save(stockRole);
                        log.info("Created Stock Clerk role");
                }

                log.info("Roles initialization completed");
        }

        private void initializeAdminUser() {
                log.info("Initializing admin user...");

                if (!userRepository.findByEmailIgnoreCase("admin@retailup.com").isPresent()) {
                        Role adminRole = roleRepository.findByNameIgnoreCase("Admin")
                                        .orElseThrow(() -> new RuntimeException("Admin role not found"));

                        User adminUser = User.builder()
                                        .name("System Administrator")
                                        .email("admin@retailup.com")
                                        .phone("+1234567890")
                                        .roles(new HashSet<>(List.of(adminRole)))
                                        .status(User.UserStatus.ACTIVE)
                                        .department("IT")
                                        .employeeId("ADMIN001")
                                        .passwordHash(passwordEncoder.encode("Admin@123"))
                                        .mustChangePassword(false)
                                        .build();

                        userRepository.save(adminUser);
                        log.info("Created admin user with email: admin@retailup.com and password: Admin@123");
                }

                log.info("Admin user initialization completed");
        }

        private Permission createPermission(String name, String label, String description,
                        Permission.PermissionCategory category) {
                return Permission.builder()
                                .name(name)
                                .label(label)
                                .description(description)
                                .category(category)
                                .isSystem(true)
                                .build();
        }
}