import type {
    User,
    Permission,
    UserStatus,
    PermissionCategory,
    UserActivity,
    UserFilters
} from '../../types/auth';

/**
 * Format user display name
 * @param user - User object
 * @returns Formatted display name
 */
export function formatUserDisplayName(user: User): string {
    if (user.employeeId) {
        return `${user.name} (${user.employeeId})`;
    }
    return user.name;
}

/**
 * Format user status for display
 * @param status - User status
 * @returns Formatted status string
 */
export function formatUserStatus(status: UserStatus): string {
    switch (status) {
        case 'active':
            return 'Active';
        case 'inactive':
            return 'Inactive';
        case 'suspended':
            return 'Suspended';
        case 'pending':
            return 'Pending Activation';
        default:
            return status;
    }
}

/**
 * Get user status color for UI
 * @param status - User status
 * @returns Color string for status
 */
export function getUserStatusColor(status: UserStatus): string {
    switch (status) {
        case 'active':
            return 'green';
        case 'inactive':
            return 'gray';
        case 'suspended':
            return 'red';
        case 'pending':
            return 'yellow';
        default:
            return 'gray';
    }
}

/**
 * Check if user has specific permission
 * @param user - User object
 * @param permissionName - Permission name to check
 * @returns True if user has permission
 */
export function hasPermission(user: User, permissionName: string): boolean {
    if (!user.role || !user.role.permissions) {
        return false;
    }

    return user.role.permissions.some(permission => permission.name === permissionName);
}

/**
 * Check if user has any permission from a list
 * @param user - User object
 * @param permissionNames - Array of permission names to check
 * @returns True if user has at least one permission
 */
export function hasAnyPermission(user: User, permissionNames: string[]): boolean {
    return permissionNames.some(permission => hasPermission(user, permission));
}

/**
 * Check if user has all permissions from a list
 * @param user - User object
 * @param permissionNames - Array of permission names to check
 * @returns True if user has all permissions
 */
export function hasAllPermissions(user: User, permissionNames: string[]): boolean {
    return permissionNames.every(permission => hasPermission(user, permission));
}

/**
 * Check if user is admin
 * @param user - User object
 * @returns True if user is admin
 */
export function isAdmin(user: User): boolean {
    return hasPermission(user, 'admin.full_access') || user.role.name.toLowerCase() === 'admin';
}

/**
 * Generate user initials for avatar
 * @param user - User object
 * @returns User initials (e.g., "JD" for John Doe)
 */
export function getUserInitials(user: User): string {
    const names = user.name.trim().split(' ');
    if (names.length >= 2) {
        return (names[0][0] + names[names.length - 1][0]).toUpperCase();
    }
    return names[0].substring(0, 2).toUpperCase();
}

/**
 * Validate email format
 * @param email - Email string to validate
 * @returns True if email format is valid
 */
export function isValidEmail(email: string): boolean {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(email);
}

/**
 * Validate password strength
 * @param password - Password to validate
 * @returns Validation result with strength level and messages
 */
export function validatePasswordStrength(password: string): {
    isValid: boolean;
    strength: 'weak' | 'medium' | 'strong';
    messages: string[];
} {
    const messages: string[] = [];
    let score = 0;

    // Length check
    if (password.length < 8) {
        messages.push('Password must be at least 8 characters long');
    } else {
        score += 1;
    }

    // Lowercase check
    if (!/[a-z]/.test(password)) {
        messages.push('Password must contain at least one lowercase letter');
    } else {
        score += 1;
    }

    // Uppercase check
    if (!/[A-Z]/.test(password)) {
        messages.push('Password must contain at least one uppercase letter');
    } else {
        score += 1;
    }

    // Number check
    if (!/\d/.test(password)) {
        messages.push('Password must contain at least one number');
    } else {
        score += 1;
    }

    // Special character check
    if (!/[!@#$%^&*(),.?":{}|<>]/.test(password)) {
        messages.push('Password must contain at least one special character');
    } else {
        score += 1;
    }

    let strength: 'weak' | 'medium' | 'strong' = 'weak';
    if (score >= 4) {
        strength = 'strong';
    } else if (score >= 3) {
        strength = 'medium';
    }

    return {
        isValid: messages.length === 0,
        strength,
        messages
    };
}

/**
 * Generate secure random password
 * @param length - Password length (default: 12)
 * @returns Generated password
 */
export function generateSecurePassword(length: number = 12): string {
    const lowercase = 'abcdefghijklmnopqrstuvwxyz';
    const uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    const numbers = '0123456789';
    const symbols = '!@#$%^&*()_+-=[]{}|;:,.<>?';

    const allChars = lowercase + uppercase + numbers + symbols;
    let password = '';

    // Ensure at least one character from each category
    password += lowercase[Math.floor(Math.random() * lowercase.length)];
    password += uppercase[Math.floor(Math.random() * uppercase.length)];
    password += numbers[Math.floor(Math.random() * numbers.length)];
    password += symbols[Math.floor(Math.random() * symbols.length)];

    // Fill remaining length
    for (let i = 4; i < length; i++) {
        password += allChars[Math.floor(Math.random() * allChars.length)];
    }

    // Shuffle the password
    return password.split('').sort(() => Math.random() - 0.5).join('');
}

/**
 * Filter users based on search criteria
 * @param users - Array of users to filter
 * @param query - Search query
 * @returns Filtered users array
 */
export function filterUsersByQuery(users: User[], query: string): User[] {
    if (!query.trim()) {
        return users;
    }

    const searchTerm = query.toLowerCase();
    return users.filter(user =>
        user.name.toLowerCase().includes(searchTerm) ||
        user.email.toLowerCase().includes(searchTerm) ||
        user.role.name.toLowerCase().includes(searchTerm) ||
        user.department?.toLowerCase().includes(searchTerm) ||
        user.employeeId?.toLowerCase().includes(searchTerm)
    );
}

/**
 * Sort users array
 * @param users - Array of users to sort
 * @param sortBy - Field to sort by
 * @param sortOrder - Sort direction
 * @returns Sorted users array
 */
export function sortUsers(
    users: User[],
    sortBy: UserFilters['sortBy'],
    sortOrder: 'asc' | 'desc' = 'asc'
): User[] {
    const sortedUsers = [...users];

    sortedUsers.sort((a, b) => {
        let aValue: string | number | Date;
        let bValue: string | number | Date;

        switch (sortBy) {
            case 'name':
                aValue = a.name.toLowerCase();
                bValue = b.name.toLowerCase();
                break;
            case 'email':
                aValue = a.email.toLowerCase();
                bValue = b.email.toLowerCase();
                break;
            case 'role':
                aValue = a.role.name.toLowerCase();
                bValue = b.role.name.toLowerCase();
                break;
            case 'status':
                aValue = a.status;
                bValue = b.status;
                break;
            case 'lastLoginAt':
                aValue = a.lastLoginAt ? new Date(a.lastLoginAt).getTime() : 0;
                bValue = b.lastLoginAt ? new Date(b.lastLoginAt).getTime() : 0;
                break;
            case 'createdAt':
                aValue = new Date(a.createdAt).getTime();
                bValue = new Date(b.createdAt).getTime();
                break;
            default:
                aValue = a.name.toLowerCase();
                bValue = b.name.toLowerCase();
        }

        if (aValue < bValue) {
            return sortOrder === 'asc' ? -1 : 1;
        }
        if (aValue > bValue) {
            return sortOrder === 'asc' ? 1 : -1;
        }
        return 0;
    });

    return sortedUsers;
}

/**
 * Get permissions by category
 * @param permissions - Array of permissions
 * @param category - Permission category
 * @returns Filtered permissions array
 */
export function getPermissionsByCategory(permissions: Permission[], category: PermissionCategory): Permission[] {
    return permissions.filter(permission => permission.category === category);
}

/**
 * Group permissions by category
 * @param permissions - Array of permissions
 * @returns Permissions grouped by category
 */
export function groupPermissionsByCategory(permissions: Permission[]): Record<PermissionCategory, Permission[]> {
    const grouped = {} as Record<PermissionCategory, Permission[]>;

    permissions.forEach(permission => {
        if (!grouped[permission.category]) {
            grouped[permission.category] = [];
        }
        grouped[permission.category].push(permission);
    });

    return grouped;
}

/**
 * Format last login time
 * @param lastLoginAt - Last login date
 * @returns Formatted time string
 */
export function formatLastLogin(lastLoginAt: Date | undefined): string {
    if (!lastLoginAt) {
        return 'Never';
    }

    const now = new Date();
    const diff = now.getTime() - new Date(lastLoginAt).getTime();
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));
    const hours = Math.floor(diff / (1000 * 60 * 60));
    const minutes = Math.floor(diff / (1000 * 60));

    if (days > 0) {
        return `${days} day${days > 1 ? 's' : ''} ago`;
    } else if (hours > 0) {
        return `${hours} hour${hours > 1 ? 's' : ''} ago`;
    } else if (minutes > 0) {
        return `${minutes} minute${minutes > 1 ? 's' : ''} ago`;
    } else {
        return 'Just now';
    }
}

/**
 * Format date time for display
 * @param date - Date to format
 * @returns Formatted date time string
 */
export function formatDateTime(date: Date): string {
    return new Intl.DateTimeFormat('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit'
    }).format(new Date(date));
}

/**
 * Format date for display
 * @param date - Date to format
 * @returns Formatted date string
 */
export function formatDate(date: Date): string {
    return new Intl.DateTimeFormat('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric'
    }).format(new Date(date));
}

/**
 * Generate unique user ID
 * @returns Unique user ID
 */
export function generateUserId(): string {
    return `user-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Generate unique role ID
 * @returns Unique role ID
 */
export function generateRoleId(): string {
    return `role-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Generate unique activity ID
 * @returns Unique activity ID
 */
export function generateActivityId(): string {
    return `activity-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Generate session token
 * @returns Session token
 */
export function generateSessionToken(): string {
    return `session_${Date.now()}_${Math.random().toString(36).substr(2, 20)}`;
}

/**
 * Check if session is expired
 * @param expiresAt - Session expiry date
 * @returns True if session is expired
 */
export function isSessionExpired(expiresAt: Date): boolean {
    return new Date() > new Date(expiresAt);
}

/**
 * Get user activity icon
 * @param activity - User activity
 * @returns Icon name for the activity
 */
export function getActivityIcon(activity: UserActivity): string {
    switch (activity.action) {
        case 'login':
            return 'LogIn';
        case 'logout':
            return 'LogOut';
        case 'create_user':
            return 'UserPlus';
        case 'update_user':
            return 'UserCheck';
        case 'delete_user':
            return 'UserX';
        case 'create_role':
            return 'Shield';
        case 'update_role':
            return 'ShieldCheck';
        case 'delete_role':
            return 'ShieldX';
        case 'change_password':
            return 'Key';
        case 'reset_password':
            return 'KeyRound';
        case 'access_denied':
            return 'AlertTriangle';
        default:
            return 'Activity';
    }
} 