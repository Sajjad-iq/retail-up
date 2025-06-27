import { useMemo } from 'react';
import { useAuthStore } from '../store/auth-store';
import {
    formatUserDisplayName,
    formatUserStatus,
    getUserStatusColor,
    hasPermission,
    hasAnyPermission,
    hasAllPermissions,
    isAdmin,
    getUserInitials,
    filterUsersByQuery,
    sortUsers,
    formatLastLogin,
    formatDateTime,
    formatDate,
    groupPermissionsByCategory
} from '../lib/utils/auth-utils';
import type {
    User,
    Role,
    Permission,
    LoginCredentials,
    UserFormData,
    RoleFormData,
    PasswordChangeFormData,
    UserFilters,
    AuthAnalytics
} from '../types/auth';

/**
 * Custom hook for authentication operations
 * Provides authentication state and login/logout functionality
 */
export function useAuth() {
    const {
        currentUser,
        session,
        loading,
        errors,
        login,
        logout,
        checkSession,
        refreshSession,
        logActivity
    } = useAuthStore();

    const computedValues = useMemo(() => ({
        isAuthenticated: !!currentUser && !!session,
        isLoading: loading.auth,
        authError: errors.auth,
        userDisplayName: currentUser ? formatUserDisplayName(currentUser) : '',
        userInitials: currentUser ? getUserInitials(currentUser) : '',
        isSessionValid: checkSession(),
        mustChangePassword: currentUser?.mustChangePassword || false
    }), [currentUser, session, loading.auth, errors.auth]);

    const loginWithValidation = async (credentials: LoginCredentials) => {
        try {
            const result = await login(credentials);
            return result;
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Login failed'
            };
        }
    };

    const hasUserPermission = (permissionName: string) => {
        return currentUser ? hasPermission(currentUser, permissionName) : false;
    };

    const hasUserAnyPermission = (permissionNames: string[]) => {
        return currentUser ? hasAnyPermission(currentUser, permissionNames) : false;
    };

    const hasUserAllPermissions = (permissionNames: string[]) => {
        return currentUser ? hasAllPermissions(currentUser, permissionNames) : false;
    };

    const isUserAdmin = () => {
        return currentUser ? isAdmin(currentUser) : false;
    };

    return {
        // State
        currentUser,
        session,
        ...computedValues,

        // Actions
        login: loginWithValidation,
        logout,
        refreshSession,
        logActivity,

        // Permission checks
        hasPermission: hasUserPermission,
        hasAnyPermission: hasUserAnyPermission,
        hasAllPermissions: hasUserAllPermissions,
        isAdmin: isUserAdmin,
    };
}

/**
 * Custom hook for user management operations
 * Provides user state and management functionality
 */
export function useUsers() {
    const {
        users,
        currentUser,
        loading,
        errors,
        addUser,
        updateUser,
        deleteUser,
        changePassword,
        resetPassword,
        getUserById,
        getActiveUsers
    } = useAuthStore();

    const computedValues = useMemo(() => ({
        totalUsers: users.length,
        activeUsers: getActiveUsers(),
        activeUserCount: getActiveUsers().length,
        inactiveUsers: users.filter(u => u.status === 'inactive'),
        suspendedUsers: users.filter(u => u.status === 'suspended'),
        pendingUsers: users.filter(u => u.status === 'pending'),
        usersRequiringPasswordChange: users.filter(u => u.mustChangePassword),
    }), [users, getActiveUsers]);

    const filterUsers = (filters: UserFilters) => {
        let filteredUsers = [...users];

        // Apply search query filter
        if (filters.query) {
            filteredUsers = filterUsersByQuery(filteredUsers, filters.query);
        }

        // Apply role filter
        if (filters.role && filters.role !== 'all') {
            filteredUsers = filteredUsers.filter(user => user.role.id === filters.role);
        }

        // Apply status filter
        if (filters.status) {
            filteredUsers = filteredUsers.filter(user => user.status === filters.status);
        }

        // Apply department filter
        if (filters.department && filters.department !== 'all') {
            filteredUsers = filteredUsers.filter(user =>
                user.department === filters.department
            );
        }

        // Apply sorting
        if (filters.sortBy) {
            filteredUsers = sortUsers(
                filteredUsers,
                filters.sortBy,
                filters.sortOrder || 'asc'
            );
        }

        return filteredUsers;
    };

    const addUserWithValidation = async (userData: UserFormData) => {
        try {
            const user = await addUser(userData);
            return { success: true, user };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to add user'
            };
        }
    };

    const updateUserWithValidation = async (id: string, userData: Partial<UserFormData>) => {
        try {
            const user = await updateUser(id, userData);
            return { success: true, user };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to update user'
            };
        }
    };

    const deleteUserWithValidation = async (id: string) => {
        try {
            await deleteUser(id);
            return { success: true };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to delete user'
            };
        }
    };

    const changePasswordWithValidation = async (userId: string, passwordData: PasswordChangeFormData) => {
        try {
            await changePassword(userId, passwordData);
            return { success: true };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to change password'
            };
        }
    };

    const resetPasswordWithValidation = async (userId: string) => {
        try {
            const tempPassword = await resetPassword(userId);
            return { success: true, tempPassword };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to reset password'
            };
        }
    };

    return {
        // State
        users,
        ...computedValues,
        loading: loading.users || loading.saving,
        error: errors.users,

        // Actions
        addUser: addUserWithValidation,
        updateUser: updateUserWithValidation,
        deleteUser: deleteUserWithValidation,
        changePassword: changePasswordWithValidation,
        resetPassword: resetPasswordWithValidation,
        getUserById,
        filterUsers,
    };
}

/**
 * Custom hook for role management operations
 * Provides role state and management functionality
 */
export function useRoles() {
    const {
        roles,
        permissions,
        users,
        loading,
        errors,
        addRole,
        updateRole,
        deleteRole,
        getRoleById,
        getUsersByRole
    } = useAuthStore();

    const computedValues = useMemo(() => {
        const rolesWithCounts = roles.map(role => ({
            ...role,
            userCount: getUsersByRole(role.id).length,
            isInUse: getUsersByRole(role.id).length > 0
        }));

        return {
            totalRoles: roles.length,
            systemRoles: roles.filter(r => r.isSystem),
            customRoles: roles.filter(r => !r.isSystem),
            rolesWithCounts,
            groupedPermissions: groupPermissionsByCategory(permissions)
        };
    }, [roles, permissions, users, getUsersByRole]);

    const addRoleWithValidation = async (roleData: RoleFormData) => {
        try {
            const role = await addRole(roleData);
            return { success: true, role };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to add role'
            };
        }
    };

    const updateRoleWithValidation = async (id: string, roleData: Partial<RoleFormData>) => {
        try {
            const role = await updateRole(id, roleData);
            return { success: true, role };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to update role'
            };
        }
    };

    const deleteRoleWithValidation = async (id: string) => {
        try {
            await deleteRole(id);
            return { success: true };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to delete role'
            };
        }
    };

    return {
        // State
        roles,
        permissions,
        ...computedValues,
        loading: loading.roles || loading.saving,
        error: errors.roles,

        // Actions
        addRole: addRoleWithValidation,
        updateRole: updateRoleWithValidation,
        deleteRole: deleteRoleWithValidation,
        getRoleById,
        getUsersByRole,
    };
}

/**
 * Custom hook for user activities and audit trail
 * Provides activity logging and history functionality
 */
export function useUserActivities() {
    const {
        activities,
        currentUser,
        getTodaysActivities,
        logActivity
    } = useAuthStore();

    const computedValues = useMemo(() => {
        const todaysActivities = getTodaysActivities();
        const recentActivities = activities.slice(0, 50);

        // Activity statistics
        const loginActivities = todaysActivities.filter(a => a.action === 'login');
        const userManagementActivities = todaysActivities.filter(a =>
            ['create_user', 'update_user', 'delete_user'].includes(a.action)
        );

        return {
            totalActivities: activities.length,
            todaysActivities,
            recentActivities,
            todaysLoginCount: loginActivities.length,
            todaysUserManagementCount: userManagementActivities.length,
        };
    }, [activities, getTodaysActivities]);

    const getActivitiesByUser = (userId: string) => {
        return activities.filter(activity => activity.userId === userId);
    };

    const getActivitiesByAction = (action: string) => {
        return activities.filter(activity => activity.action === action);
    };

    const getActivitiesByDateRange = (startDate: Date, endDate: Date) => {
        return activities.filter(activity => {
            const activityDate = new Date(activity.timestamp);
            return activityDate >= startDate && activityDate <= endDate;
        });
    };

    return {
        // State
        activities,
        ...computedValues,

        // Actions
        logActivity,
        getActivitiesByUser,
        getActivitiesByAction,
        getActivitiesByDateRange,
    };
}

/**
 * Custom hook for auth analytics and statistics
 * Provides computed analytics and statistics for the admin dashboard
 */
export function useAuthAnalytics() {
    const {
        users,
        activities,
        roles,
        getActiveUsers,
        getTodaysActivities
    } = useAuthStore();

    const analytics = useMemo(() => {
        const activeUsers = getActiveUsers();
        const todaysActivities = getTodaysActivities();

        // Login statistics
        const todaysLogins = todaysActivities.filter(a => a.action === 'login');
        const uniqueLoginsToday = new Set(todaysLogins.map(a => a.userId)).size;

        // New users this month
        const thisMonth = new Date();
        thisMonth.setDate(1);
        thisMonth.setHours(0, 0, 0, 0);
        const newUsersThisMonth = users.filter(u => new Date(u.createdAt) >= thisMonth).length;

        // Most active users (by activity count in last 30 days)
        const thirtyDaysAgo = new Date();
        thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);
        const recentActivities = activities.filter(a => new Date(a.timestamp) >= thirtyDaysAgo);

        const userActivityCounts = recentActivities.reduce((acc, activity) => {
            acc[activity.userId] = (acc[activity.userId] || 0) + 1;
            return acc;
        }, {} as Record<string, number>);

        const mostActiveUsers = Object.entries(userActivityCounts)
            .sort(([, a], [, b]) => b - a)
            .slice(0, 5)
            .map(([userId, count]) => {
                const user = users.find(u => u.id === userId);
                return user ? { ...user, activityCount: count } : null;
            })
            .filter(Boolean) as (User & { activityCount: number })[];

        // Role distribution
        const roleCounts = users.reduce((acc, user) => {
            acc[user.role.name] = (acc[user.role.name] || 0) + 1;
            return acc;
        }, {} as Record<string, number>);

        const roleDistribution = Object.entries(roleCounts).map(([role, count]) => ({
            role,
            count,
            percentage: Math.round((count / users.length) * 100)
        }));

        // Login activity by day (last 7 days)
        const loginActivity = Array.from({ length: 7 }, (_, i) => {
            const date = new Date();
            date.setDate(date.getDate() - i);
            const dateString = date.toDateString();

            const dayLogins = activities.filter(a =>
                a.action === 'login' &&
                new Date(a.timestamp).toDateString() === dateString
            ).length;

            return {
                date: date.toISOString().split('T')[0],
                count: dayLogins
            };
        }).reverse();

        const authAnalytics: AuthAnalytics = {
            totalUsers: users.length,
            activeUsers: activeUsers.length,
            loginsToday: uniqueLoginsToday,
            newUsersThisMonth,
            mostActiveUsers,
            roleDistribution,
            loginActivity
        };

        return authAnalytics;
    }, [users, activities, roles, getActiveUsers, getTodaysActivities]);

    return analytics;
}

/**
 * Custom hook for auth formatting utilities
 * Provides formatting functions for the auth system
 */
export function useAuthFormatters() {
    return {
        formatUserDisplayName,
        formatUserStatus,
        getUserStatusColor,
        getUserInitials,
        formatLastLogin,
        formatDateTime,
        formatDate,
        hasPermission: (user: User, permission: string) => hasPermission(user, permission),
        hasAnyPermission: (user: User, permissions: string[]) => hasAnyPermission(user, permissions),
        hasAllPermissions: (user: User, permissions: string[]) => hasAllPermissions(user, permissions),
        isAdmin: (user: User) => isAdmin(user),
    };
} 