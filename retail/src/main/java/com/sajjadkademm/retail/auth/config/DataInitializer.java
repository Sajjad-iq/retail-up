package com.sajjadkademm.retail.auth.config;

import com.sajjadkademm.retail.auth.entities.Permission;
import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.repositories.PermissionRepository;
import com.sajjadkademm.retail.auth.repositories.UserRepository;
import com.sajjadkademm.retail.auth.utils.AuthUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Data initializer for setting up default permissions and admin user.
 * Runs on application startup to ensure required data exists.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DataInitializer implements CommandLineRunner {

        private final PermissionRepository permissionRepository;
        private final UserRepository userRepository;
        private final PasswordEncoder passwordEncoder;

        @Override
        public void run(String... args) throws Exception {
                log.info("Starting data initialization...");

                try {
                        initializePermissions();
                        initializeAdminUser();
                        log.info("Data initialization completed successfully");
                } catch (Exception e) {
                        log.error("Data initialization failed: {}", e.getMessage(), e);
                }
        }

        /**
         * Initialize default permissions based on React frontend permissions
         */
        private void initializePermissions() {
                log.info("Initializing permissions...");

                List<PermissionData> permissionsData = Arrays.asList(
                                // POS Permissions
                                new PermissionData("pos.access", "POS Access", "Access to Point of Sale system",
                                                Permission.PermissionCategory.POS, false),
                                new PermissionData("pos.sell", "POS Sell", "Create sales transactions",
                                                Permission.PermissionCategory.POS, false),
                                new PermissionData("pos.refund", "POS Refund", "Process refunds",
                                                Permission.PermissionCategory.POS,
                                                false),
                                new PermissionData("pos.hold", "POS Hold Transaction", "Hold and resume transactions",
                                                Permission.PermissionCategory.POS, false),
                                new PermissionData("pos.discount", "POS Discount", "Apply discounts to transactions",
                                                Permission.PermissionCategory.POS, false),

                                // Inventory Permissions
                                new PermissionData("inventory.view", "View Inventory",
                                                "View inventory items and stock levels",
                                                Permission.PermissionCategory.INVENTORY, false),
                                new PermissionData("inventory.manage", "Manage Inventory",
                                                "Add, edit, and remove inventory items",
                                                Permission.PermissionCategory.INVENTORY, false),
                                new PermissionData("inventory.adjust", "Adjust Stock", "Adjust inventory stock levels",
                                                Permission.PermissionCategory.INVENTORY, false),
                                new PermissionData("inventory.movements", "View Stock Movements",
                                                "View stock movement history",
                                                Permission.PermissionCategory.INVENTORY, false),
                                new PermissionData("inventory.alerts", "Inventory Alerts",
                                                "Manage low stock and expiry alerts",
                                                Permission.PermissionCategory.INVENTORY, false),

                                // Users Permissions
                                new PermissionData("users.view", "View Users", "View user accounts and information",
                                                Permission.PermissionCategory.USERS, false),
                                new PermissionData("users.manage", "Manage Users",
                                                "Create, edit, and delete user accounts",
                                                Permission.PermissionCategory.USERS, false),
                                new PermissionData("users.permissions", "Manage User Permissions",
                                                "Assign and revoke user permissions",
                                                Permission.PermissionCategory.USERS, false),
                                new PermissionData("users.activities", "View User Activities",
                                                "View user activity logs",
                                                Permission.PermissionCategory.USERS, false),

                                // Reports Permissions
                                new PermissionData("reports.view", "View Reports", "Access reporting and analytics",
                                                Permission.PermissionCategory.REPORTS, false),
                                new PermissionData("reports.export", "Export Reports", "Export reports and data",
                                                Permission.PermissionCategory.REPORTS, false),
                                new PermissionData("reports.financial", "Financial Reports", "Access financial reports",
                                                Permission.PermissionCategory.REPORTS, false),
                                new PermissionData("reports.sales", "Sales Reports", "Access sales analytics",
                                                Permission.PermissionCategory.REPORTS, false),

                                // Settings Permissions
                                new PermissionData("settings.view", "View Settings", "View system settings",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.business", "Business Settings",
                                                "Manage business configuration",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.pos", "POS Settings", "Configure POS system settings",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.inventory", "Inventory Settings",
                                                "Configure inventory settings",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.payments", "Payment Settings", "Configure payment methods",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.notifications", "Notification Settings",
                                                "Manage notification preferences",
                                                Permission.PermissionCategory.SETTINGS, false),
                                new PermissionData("settings.integrations", "Integration Settings",
                                                "Manage third-party integrations",
                                                Permission.PermissionCategory.SETTINGS, false),

                                // Admin Permissions
                                new PermissionData("admin.full_access", "Full Admin Access",
                                                "Complete system administration access",
                                                Permission.PermissionCategory.ADMIN, true),
                                new PermissionData("admin.system", "System Administration",
                                                "System-level administrative functions",
                                                Permission.PermissionCategory.ADMIN, true),
                                new PermissionData("admin.backup", "Backup & Restore",
                                                "Database backup and restore operations",
                                                Permission.PermissionCategory.ADMIN, true),
                                new PermissionData("admin.logs", "System Logs", "Access system logs and diagnostics",
                                                Permission.PermissionCategory.ADMIN, true));

                for (PermissionData permData : permissionsData) {
                        if (!permissionRepository.existsByName(permData.name)) {
                                Permission permission = Permission.builder()
                                                .id(AuthUtils.generatePermissionId())
                                                .name(permData.name)
                                                .label(permData.label)
                                                .description(permData.description)
                                                .category(permData.category)
                                                .isSystem(permData.isSystem)
                                                .build();

                                permissionRepository.save(permission);
                                log.debug("Created permission: {}", permData.name);
                        }
                }

                log.info("Permissions initialization completed. Total permissions: {}", permissionRepository.count());
        }

        /**
         * Initialize default admin user
         */
        private void initializeAdminUser() {
                log.info("Initializing admin user...");

                String adminEmail = "admin@retailup.com";

                if (!userRepository.existsByEmail(adminEmail)) {
                        // Get all permissions for admin user
                        List<Permission> allPermissions = permissionRepository.findAll();
                        Set<Permission> adminPermissions = new HashSet<>(allPermissions);

                        User adminUser = User.builder()
                                        .id(AuthUtils.generateUserId())
                                        .name("System Administrator")
                                        .email(adminEmail)
                                        .password(passwordEncoder.encode("admin123")) // Default password - should be
                                                                                      // changed on first login
                                        .phone("+1234567890")
                                        .department("IT")
                                        .employeeId("ADMIN001")
                                        .status(User.UserStatus.ACTIVE)
                                        .mustChangePassword(false) // Allow initial login - admin can change password
                                                                   // through settings
                                        .permissions(adminPermissions)
                                        .build();

                        userRepository.save(adminUser);
                        log.info("Admin user created with email: {} (default password: admin123)", adminEmail);
                        log.warn("SECURITY WARNING: Please change the default admin password immediately!");
                } else {
                        log.info("Admin user already exists with email: {}", adminEmail);
                }
        }

        /**
         * Helper class for permission data
         */
        private static class PermissionData {
                final String name;
                final String label;
                final String description;
                final Permission.PermissionCategory category;
                final boolean isSystem;

                PermissionData(String name, String label, String description, Permission.PermissionCategory category,
                                boolean isSystem) {
                        this.name = name;
                        this.label = label;
                        this.description = description;
                        this.category = category;
                        this.isSystem = isSystem;
                }
        }
}