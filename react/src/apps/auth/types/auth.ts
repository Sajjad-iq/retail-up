/**
 * User interface representing system users
 */
export interface User {
    /** Unique identifier for the user */
    id: string;
    /** User's display name */
    name: string;
    /** User's email address */
    email: string;
    /** User's phone number */
    phone?: string;
    /** User's avatar URL */
    avatar?: string;
    /** User's role */
    role: Role;
    /** User's status */
    status: UserStatus;
    /** User's department */
    department?: string;
    /** Employee ID */
    employeeId?: string;
    /** Date when user was created */
    createdAt: Date;
    /** Date when user was last updated */
    updatedAt: Date;
    /** Date when user last logged in */
    lastLoginAt?: Date;
    /** Whether user must change password on next login */
    mustChangePassword: boolean;
}

/**
 * Role interface representing user roles and permissions
 */
export interface Role {
    /** Unique identifier for the role */
    id: string;
    /** Role name */
    name: string;
    /** Role description */
    description?: string;
    /** Role color for UI */
    color?: string;
    /** Permissions assigned to this role */
    permissions: Permission[];
    /** Whether this is a system role (cannot be deleted) */
    isSystem: boolean;
    /** Date when role was created */
    createdAt: Date;
    /** Date when role was last updated */
    updatedAt: Date;
}

/**
 * Permission interface representing specific system permissions
 */
export interface Permission {
    /** Unique identifier for the permission */
    id: string;
    /** Permission name/code */
    name: string;
    /** Human-readable permission label */
    label: string;
    /** Permission description */
    description?: string;
    /** Permission category/module */
    category: PermissionCategory;
    /** Whether this is a system permission */
    isSystem: boolean;
}

/**
 * User status types
 */
export type UserStatus = 'active' | 'inactive' | 'suspended' | 'pending';

/**
 * Permission category types
 */
export type PermissionCategory =
    | 'pos'
    | 'inventory'
    | 'users'
    | 'reports'
    | 'settings'
    | 'admin';

/**
 * Authentication session
 */
export interface AuthSession {
    /** Session token */
    token: string;
    /** Authenticated user */
    user: User;
    /** Session expiry date */
    expiresAt: Date;
    /** Session creation date */
    createdAt: Date;
}

/**
 * Login credentials
 */
export interface LoginCredentials {
    /** Email address */
    email: string;
    /** Password */
    password: string;
    /** Remember me option */
    rememberMe?: boolean;
}

/**
 * User activity log
 */
export interface UserActivity {
    /** Unique activity identifier */
    id: string;
    /** User who performed the action */
    userId: string;
    /** User details */
    user: User;
    /** Action performed */
    action: UserAction;
    /** Resource affected */
    resource?: string;
    /** Additional details */
    details?: string;
    /** IP address */
    ipAddress?: string;
    /** User agent */
    userAgent?: string;
    /** Activity timestamp */
    timestamp: Date;
}

/**
 * User action types
 */
export type UserAction =
    | 'login'
    | 'logout'
    | 'create_user'
    | 'update_user'
    | 'delete_user'
    | 'create_role'
    | 'update_role'
    | 'delete_role'
    | 'change_password'
    | 'reset_password'
    | 'access_denied';

/**
 * Password reset request
 */
export interface PasswordResetRequest {
    /** Unique request identifier */
    id: string;
    /** User's email */
    email: string;
    /** Reset token */
    token: string;
    /** Request expiry date */
    expiresAt: Date;
    /** Whether request has been used */
    used: boolean;
    /** Request creation date */
    createdAt: Date;
}

/**
 * Auth store state interface
 */
export interface AuthState {
    /** Current authenticated user */
    currentUser: User | null;
    /** Current session */
    session: AuthSession | null;
    /** All users (admin view) */
    users: User[];
    /** All roles */
    roles: Role[];
    /** All permissions */
    permissions: Permission[];
    /** User activities */
    activities: UserActivity[];
    /** Loading states */
    loading: {
        auth: boolean;
        users: boolean;
        roles: boolean;
        saving: boolean;
    };
    /** Error states */
    errors: {
        auth: string | null;
        users: string | null;
        roles: string | null;
    };
}

/**
 * Form data for creating/editing users
 */
export interface UserFormData {
    /** User name */
    name: string;
    /** Email address */
    email: string;
    /** Phone number */
    phone?: string;
    /** Department */
    department?: string;
    /** Employee ID */
    employeeId?: string;
    /** Role ID */
    roleId: string;
    /** User status */
    status: UserStatus;
    /** Password (for new users) */
    password?: string;
    /** Force password change */
    mustChangePassword: boolean;
}

/**
 * Form data for creating/editing roles
 */
export interface RoleFormData {
    /** Role name */
    name: string;
    /** Role description */
    description?: string;
    /** Role color */
    color?: string;
    /** Permission IDs */
    permissionIds: string[];
}

/**
 * Password change form data
 */
export interface PasswordChangeFormData {
    /** Current password */
    currentPassword: string;
    /** New password */
    newPassword: string;
    /** Confirm new password */
    confirmPassword: string;
}

/**
 * User filters for admin dashboard
 */
export interface UserFilters {
    /** Search query */
    query?: string;
    /** Role filter */
    role?: string;
    /** Status filter */
    status?: UserStatus;
    /** Department filter */
    department?: string;
    /** Sort by field */
    sortBy?: 'name' | 'email' | 'role' | 'status' | 'lastLoginAt' | 'createdAt';
    /** Sort direction */
    sortOrder?: 'asc' | 'desc';
}

/**
 * Auth analytics data
 */
export interface AuthAnalytics {
    /** Total users count */
    totalUsers: number;
    /** Active users count */
    activeUsers: number;
    /** Users logged in today */
    loginsToday: number;
    /** New users this month */
    newUsersThisMonth: number;
    /** Most active users */
    mostActiveUsers: User[];
    /** Role distribution */
    roleDistribution: RoleDistribution[];
    /** Login activity by day */
    loginActivity: LoginActivityData[];
}

/**
 * Role distribution data
 */
export interface RoleDistribution {
    /** Role name */
    role: string;
    /** User count */
    count: number;
    /** Percentage */
    percentage: number;
}

/**
 * Login activity data
 */
export interface LoginActivityData {
    /** Date */
    date: string;
    /** Login count */
    count: number;
} 