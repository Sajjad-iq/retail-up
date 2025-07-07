import { create } from 'zustand';
import { devtools, persist } from 'zustand/middleware';
import type {
    User,
    Role,
    Permission,
    AuthSession,
    UserActivity,
    LoginCredentials,
    UserFormData,
    PasswordChangeFormData,
    UserAction
} from '../types/auth';
import {
    generateUserId,
    generateRoleId,
    generateActivityId,
    generateSessionToken,
    isSessionExpired,
} from '../lib/utils/auth-utils';

/**
 * Sample permissions data for demonstration
 */
const samplePermissions: Permission[] = [
    // POS Permissions
    {
        id: 'perm-pos-001',
        name: 'pos.view',
        label: 'View POS',
        description: 'Access to view POS interface',
        category: 'pos',
        isSystem: true
    },
    {
        id: 'perm-pos-002',
        name: 'pos.sell',
        label: 'Process Sales',
        description: 'Process sales transactions',
        category: 'pos',
        isSystem: true
    },
    {
        id: 'perm-pos-003',
        name: 'pos.refund',
        label: 'Process Refunds',
        description: 'Process refunds and returns',
        category: 'pos',
        isSystem: true
    },
    {
        id: 'perm-pos-004',
        name: 'pos.discount',
        label: 'Apply Discounts',
        description: 'Apply discounts to transactions',
        category: 'pos',
        isSystem: true
    },

    // Inventory Permissions
    {
        id: 'perm-inv-001',
        name: 'inventory.view',
        label: 'View Inventory',
        description: 'Access to view inventory',
        category: 'inventory',
        isSystem: true
    },
    {
        id: 'perm-inv-002',
        name: 'inventory.manage',
        label: 'Manage Inventory',
        description: 'Add, edit, delete inventory items',
        category: 'inventory',
        isSystem: true
    },
    {
        id: 'perm-inv-003',
        name: 'inventory.adjust',
        label: 'Adjust Stock',
        description: 'Adjust stock levels',
        category: 'inventory',
        isSystem: true
    },

    // User Management Permissions
    {
        id: 'perm-usr-001',
        name: 'users.view',
        label: 'View Users',
        description: 'View user list and details',
        category: 'users',
        isSystem: true
    },
    {
        id: 'perm-usr-002',
        name: 'users.manage',
        label: 'Manage Users',
        description: 'Create, edit, delete users',
        category: 'users',
        isSystem: true
    },
    {
        id: 'perm-usr-003',
        name: 'users.roles',
        label: 'Manage Roles',
        description: 'Edit, delete roles',
        category: 'users',
        isSystem: true
    },

    // Reports Permissions
    {
        id: 'perm-rpt-001',
        name: 'reports.view',
        label: 'View Reports',
        description: 'Access to view reports',
        category: 'reports',
        isSystem: true
    },
    {
        id: 'perm-rpt-002',
        name: 'reports.export',
        label: 'Export Reports',
        description: 'Export reports to files',
        category: 'reports',
        isSystem: true
    },

    // Settings Permissions
    {
        id: 'perm-set-001',
        name: 'settings.view',
        label: 'View Settings',
        description: 'Access to view system settings',
        category: 'settings',
        isSystem: true
    },
    {
        id: 'perm-set-002',
        name: 'settings.manage',
        label: 'Manage Settings',
        description: 'Modify system settings',
        category: 'settings',
        isSystem: true
    },

    // Admin Permissions
    {
        id: 'perm-adm-001',
        name: 'admin.full_access',
        label: 'Full Admin Access',
        description: 'Complete system administration access',
        category: 'admin',
        isSystem: true
    }
];

/**
 * Sample roles data for demonstration
 */
const sampleRoles: Role[] = [
    {
        id: 'role-001',
        name: 'Admin',
        description: 'System administrator with full access',
        color: '#DC2626',
        permissions: samplePermissions,
        isSystem: true,
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date('2024-01-01')
    },
    {
        id: 'role-002',
        name: 'Manager',
        description: 'Store manager with most permissions',
        color: '#7C3AED',
        permissions: samplePermissions.filter(p => !p.name.includes('admin.')),
        isSystem: true,
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date('2024-01-01')
    },
    {
        id: 'role-003',
        name: 'Cashier',
        description: 'Front-of-house staff for POS operations',
        color: '#059669',
        permissions: samplePermissions.filter(p =>
            p.category === 'pos' ||
            (p.category === 'inventory' && p.name === 'inventory.view')
        ),
        isSystem: true,
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date('2024-01-01')
    },
    {
        id: 'role-004',
        name: 'Stock Clerk',
        description: 'Inventory management staff',
        color: '#DC7D00',
        permissions: samplePermissions.filter(p =>
            p.category === 'inventory' ||
            (p.category === 'pos' && p.name === 'pos.view')
        ),
        isSystem: true,
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date('2024-01-01')
    }
];

/**
 * Sample users data for demonstration
 */
const sampleUsers: User[] = [
    {
        id: 'user-001',
        name: 'Admin User',
        email: 'admin@retailup.com',
        phone: '+1-555-0001',
        permissions: [...samplePermissions], // All permissions
        status: 'active',
        department: 'Management',
        employeeId: 'EMP001',
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date('2024-01-01'),
        lastLoginAt: new Date(),
        mustChangePassword: false
    },
    {
        id: 'user-002',
        name: 'John Manager',
        email: 'john.manager@retailup.com',
        phone: '+1-555-0002',
        permissions: samplePermissions.filter(p => !p.name.includes('admin.')), // Manager permissions
        status: 'active',
        department: 'Operations',
        employeeId: 'EMP002',
        createdAt: new Date('2024-01-15'),
        updatedAt: new Date('2024-01-15'),
        lastLoginAt: new Date(Date.now() - 2 * 60 * 60 * 1000), // 2 hours ago
        mustChangePassword: false
    },
    {
        id: 'user-003',
        name: 'Jane Cashier',
        email: 'jane.cashier@retailup.com',
        phone: '+1-555-0003',
        permissions: samplePermissions.filter(p =>
            p.category === 'pos' ||
            (p.category === 'inventory' && p.name === 'inventory.view')
        ), // Cashier permissions
        status: 'active',
        department: 'Sales',
        employeeId: 'EMP003',
        createdAt: new Date('2024-02-01'),
        updatedAt: new Date('2024-02-01'),
        lastLoginAt: new Date(Date.now() - 30 * 60 * 1000), // 30 minutes ago
        mustChangePassword: false
    },
    {
        id: 'user-004',
        name: 'Mike Stock',
        email: 'mike.stock@retailup.com',
        permissions: samplePermissions.filter(p =>
            p.category === 'inventory' ||
            (p.category === 'pos' && p.name === 'pos.view')
        ), // Stock clerk permissions
        status: 'active',
        department: 'Warehouse',
        employeeId: 'EMP004',
        createdAt: new Date('2024-02-10'),
        updatedAt: new Date('2024-02-10'),
        lastLoginAt: new Date(Date.now() - 24 * 60 * 60 * 1000), // 24 hours ago
        mustChangePassword: true
    }
];

interface AuthStore {
    // State
    currentUser: User | null;
    session: AuthSession | null;
    users: User[];
    roles: Role[];
    permissions: Permission[];
    activities: UserActivity[];
    loading: {
        auth: boolean;
        users: boolean;
        roles: boolean;
        saving: boolean;
    };
    errors: {
        auth: string | null;
        users: string | null;
        roles: string | null;
    };

    // Auth Actions
    login: (credentials: LoginCredentials) => Promise<{ success: boolean; error?: string }>;
    logout: () => void;
    checkSession: () => boolean;
    refreshSession: () => Promise<boolean>;

    // User Actions
    addUser: (userData: UserFormData) => Promise<User>;
    updateUser: (id: string, userData: Partial<UserFormData>) => Promise<User>;
    deleteUser: (id: string) => Promise<void>;
    changePassword: (userId: string, passwordData: PasswordChangeFormData) => Promise<void>;
    resetPassword: (userId: string) => Promise<string>;

    // Role Actions
    updateRole: (id: string, roleData: Partial<any>) => Promise<Role>;
    deleteRole: (id: string) => Promise<void>;

    // Activity Actions
    logActivity: (action: UserAction, resource?: string, details?: string) => void;

    // Utility Actions
    getUserById: (id: string) => User | undefined;
    getRoleById: (id: string) => Role | undefined;
    getPermissionById: (id: string) => Permission | undefined;

    // Computed getters
    getActiveUsers: () => User[];
    getTodaysActivities: () => UserActivity[];
    getUsersByRole: (roleId: string) => User[];
}

/**
 * Main Auth store using Zustand
 * Provides state management for authentication, users, roles, and permissions
 */
export const useAuthStore = create<AuthStore>()(
    devtools(
        persist(
            (set, get) => ({
                // Initial State
                currentUser: sampleUsers[0],
                session: null,
                users: sampleUsers,
                roles: sampleRoles,
                permissions: samplePermissions,
                activities: [],
                loading: {
                    auth: false,
                    users: false,
                    roles: false,
                    saving: false,
                },
                errors: {
                    auth: null,
                    users: null,
                    roles: null,
                },

                // Auth Actions
                /**
                 * Authenticate user with credentials
                 */
                login: async (credentials: LoginCredentials) => {
                    set(state => ({
                        loading: { ...state.loading, auth: true },
                        errors: { ...state.errors, auth: null }
                    }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const user = state.users.find(u =>
                                u.email.toLowerCase() === credentials.email.toLowerCase() &&
                                u.status === 'active'
                            );

                            if (user) {
                                // In a real app, you'd verify password here
                                const expiresAt = new Date();
                                expiresAt.setHours(expiresAt.getHours() + (credentials.rememberMe ? 168 : 24)); // 7 days or 24 hours

                                const session: AuthSession = {
                                    token: generateSessionToken(),
                                    user,
                                    expiresAt,
                                    createdAt: new Date()
                                };

                                const updatedUser = {
                                    ...user,
                                    lastLoginAt: new Date(),
                                    updatedAt: new Date()
                                };

                                set(state => ({
                                    currentUser: updatedUser,
                                    session,
                                    users: state.users.map(u => u.id === user.id ? updatedUser : u),
                                    loading: { ...state.loading, auth: false }
                                }));

                                // Log activity
                                get().logActivity('login');

                                resolve({ success: true });
                            } else {
                                set(state => ({
                                    loading: { ...state.loading, auth: false },
                                    errors: { ...state.errors, auth: 'Invalid email or password' }
                                }));
                                resolve({ success: false, error: 'Invalid email or password' });
                            }
                        }, 1000); // Simulate API delay
                    });
                },

                /**
                 * Logout current user
                 */
                logout: () => {
                    get().logActivity('logout');
                    set({
                        currentUser: null,
                        session: null,
                        errors: { auth: null, users: null, roles: null }
                    });
                },

                /**
                 * Check if current session is valid
                 */
                checkSession: () => {
                    const { session } = get();
                    if (!session) return false;

                    if (isSessionExpired(session.expiresAt)) {
                        get().logout();
                        return false;
                    }

                    return true;
                },

                /**
                 * Refresh current session
                 */
                refreshSession: async () => {
                    const { session, currentUser } = get();
                    if (!session || !currentUser) return false;

                    const expiresAt = new Date();
                    expiresAt.setHours(expiresAt.getHours() + 24);

                    const newSession: AuthSession = {
                        ...session,
                        expiresAt
                    };

                    set({ session: newSession });
                    return true;
                },

                // User Actions
                /**
                 * Add a new user
                 */
                addUser: async (userData: UserFormData) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();

                            // Get user permissions from selected permission IDs
                            const userPermissions = userData.permissionIds
                                .map(id => state.getPermissionById(id))
                                .filter(Boolean) as Permission[];

                            const newUser: User = {
                                id: generateUserId(),
                                name: userData.name,
                                email: userData.email,
                                phone: userData.phone,
                                permissions: userPermissions,
                                status: userData.status,
                                department: userData.department,
                                employeeId: userData.employeeId,
                                createdAt: new Date(),
                                updatedAt: new Date(),
                                mustChangePassword: userData.mustChangePassword
                            };

                            set(state => ({
                                users: [...state.users, newUser],
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('create_user', newUser.name);

                            resolve(newUser);
                        }, 500);
                    });
                },

                /**
                 * Update an existing user
                 */
                updateUser: async (id: string, userData: Partial<UserFormData>) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const userIndex = state.users.findIndex(u => u.id === id);

                            if (userIndex === -1) {
                                throw new Error('User not found');
                            }

                            const currentUser = state.users[userIndex];
                            let permissions = currentUser.permissions;

                            // Update user permissions if provided
                            if (userData.permissionIds) {
                                permissions = userData.permissionIds
                                    .map(id => state.getPermissionById(id))
                                    .filter(Boolean) as Permission[];
                            }

                            const updatedUser: User = {
                                ...currentUser,
                                ...userData,
                                permissions,
                                updatedAt: new Date()
                            };

                            const updatedUsers = [...state.users];
                            updatedUsers[userIndex] = updatedUser;

                            set(state => ({
                                users: updatedUsers,
                                currentUser: state.currentUser?.id === id ? updatedUser : state.currentUser,
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('update_user', updatedUser.name);

                            resolve(updatedUser);
                        }, 500);
                    });
                },

                /**
                 * Delete a user
                 */
                deleteUser: async (id: string) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const user = state.getUserById(id);

                            if (!user) {
                                throw new Error('User not found');
                            }

                            if (user.id === state.currentUser?.id) {
                                throw new Error('Cannot delete your own account');
                            }

                            set(state => ({
                                users: state.users.filter(u => u.id !== id),
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('delete_user', user.name);

                            resolve();
                        }, 500);
                    });
                },

                /**
                 * Change user password
                 */
                changePassword: async (userId: string) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const user = state.getUserById(userId);

                            if (!user) {
                                throw new Error('User not found');
                            }

                            // In a real app, you'd verify current password and hash the new one
                            const updatedUser = {
                                ...user,
                                mustChangePassword: false,
                                updatedAt: new Date()
                            };

                            set(state => ({
                                users: state.users.map(u => u.id === userId ? updatedUser : u),
                                currentUser: state.currentUser?.id === userId ? updatedUser : state.currentUser,
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('change_password', user.name);

                            resolve();
                        }, 500);
                    });
                },

                /**
                 * Reset user password
                 */
                resetPassword: async (userId: string) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const user = state.getUserById(userId);

                            if (!user) {
                                throw new Error('User not found');
                            }

                            // Generate temporary password
                            const tempPassword = Math.random().toString(36).substring(2, 10);

                            const updatedUser = {
                                ...user,
                                mustChangePassword: true,
                                updatedAt: new Date()
                            };

                            set(state => ({
                                users: state.users.map(u => u.id === userId ? updatedUser : u),
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('reset_password', user.name);

                            resolve(tempPassword);
                        }, 500);
                    });
                },

                // Role Actions

                /**
                 * Update an existing role
                 */
                updateRole: async (id: string, roleData: Partial<any>) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const roleIndex = state.roles.findIndex(r => r.id === id);

                            if (roleIndex === -1) {
                                throw new Error('Role not found');
                            }

                            const currentRole = state.roles[roleIndex];

                            if (currentRole.isSystem) {
                                throw new Error('Cannot modify system roles');
                            }

                            let permissions = currentRole.permissions;
                            if (roleData.permissionIds) {
                                permissions = roleData.permissionIds
                                    .map((permId: string) => state.getPermissionById(permId))
                                    .filter(Boolean) as Permission[];
                            }

                            const updatedRole: Role = {
                                ...currentRole,
                                ...roleData,
                                permissions,
                                updatedAt: new Date()
                            };

                            const updatedRoles = [...state.roles];
                            updatedRoles[roleIndex] = updatedRole;

                            // Update users with this role
                            const updatedUsers = state.users.map(user =>
                                user.permissions?.some(p => p.id === id) ? { ...user, permissions: updatedRole.permissions } : user
                            );

                            set(state => ({
                                roles: updatedRoles,
                                users: updatedUsers,
                                currentUser: state.currentUser?.permissions?.some(p => p.id === id)
                                    ? { ...state.currentUser, permissions: updatedRole.permissions }
                                    : state.currentUser,
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('update_role', updatedRole.name);

                            resolve(updatedRole);
                        }, 500);
                    });
                },

                /**
                 * Delete a role
                 */
                deleteRole: async (id: string) => {
                    set(state => ({ loading: { ...state.loading, saving: true } }));

                    return new Promise((resolve) => {
                        setTimeout(() => {
                            const state = get();
                            const role = state.getRoleById(id);

                            if (!role) {
                                throw new Error('Role not found');
                            }

                            if (role.isSystem) {
                                throw new Error('Cannot delete system roles');
                            }

                            const usersWithRole = state.users.filter(user =>
                                user.permissions?.some(p => p.id === id)
                            );
                            if (usersWithRole.length > 0) {
                                throw new Error(`Cannot delete role. ${usersWithRole.length} users are assigned to this role.`);
                            }

                            set(state => ({
                                roles: state.roles.filter(r => r.id !== id),
                                loading: { ...state.loading, saving: false }
                            }));

                            // Log activity
                            get().logActivity('delete_role', role.name);

                            resolve();
                        }, 500);
                    });
                },

                // Activity Actions
                /**
                 * Log user activity
                 */
                logActivity: (action: UserAction, resource?: string, details?: string) => {
                    const { currentUser } = get();
                    if (!currentUser) return;

                    const activity: UserActivity = {
                        id: generateActivityId(),
                        userId: currentUser.id,
                        user: currentUser,
                        action,
                        resource,
                        details,
                        timestamp: new Date()
                    };

                    set(state => ({
                        activities: [activity, ...state.activities].slice(0, 1000) // Keep last 1000 activities
                    }));
                },

                // Utility Actions
                /**
                 * Get user by ID
                 */
                getUserById: (id: string) => {
                    return get().users.find(user => user.id === id);
                },

                /**
                 * Get role by ID
                 */
                getRoleById: (id: string) => {
                    return get().roles.find(role => role.id === id);
                },

                /**
                 * Get permission by ID
                 */
                getPermissionById: (id: string) => {
                    return get().permissions.find(permission => permission.id === id);
                },

                // Computed getters
                /**
                 * Get active users
                 */
                getActiveUsers: () => {
                    return get().users.filter(user => user.status === 'active');
                },

                /**
                 * Get today's activities
                 */
                getTodaysActivities: () => {
                    const today = new Date().toDateString();
                    return get().activities.filter(
                        activity => new Date(activity.timestamp).toDateString() === today
                    );
                },

                /**
                 * Get users by role
                 */
                getUsersByRole: (roleId: string) => {
                    return get().users.filter(user => user.permissions?.some(p => p.id === roleId));
                },
            }),
            {
                name: 'auth-storage',
                partialize: (state) => ({
                    users: state.users,
                    roles: state.roles.filter(r => !r.isSystem), // Don't persist system roles
                    activities: state.activities.slice(0, 100), // Persist only recent activities
                    // Don't persist session and currentUser for security
                }),
            }
        ),
        {
            name: 'auth-store',
        }
    )
); 