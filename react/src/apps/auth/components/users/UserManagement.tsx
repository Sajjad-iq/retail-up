import { useState, useMemo } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import {
    Grid3X3,
    List,
    Search,
    Filter,
    MoreHorizontal,
    UserCheck,
    UserX,
    UserPlus,
    Mail,
    Phone,
    Building,
    Calendar,
    Key,
    RotateCcw,
    Edit,
    Trash2,
    CheckCircle,
    XCircle,
    AlertTriangle,
    Clock,
    SortAsc,
    SortDesc
} from 'lucide-react';

import { UserTable } from './UserTable';
import { UserCard } from './UserCard';
import { EditUserDialog } from '../forms/EditUserDialog';
import { ChangePasswordDialog } from '../forms/ChangePasswordDialog';
import { useUsers, useRoles, useAuth, useAuthFormatters } from '../../hooks/use-auth';
import type { User, UserFilters, UserStatus } from '../../types/auth';
import React from 'react';

/**
 * UserManagement component props
 */
interface UserManagementProps {
    /** Search query from parent component */
    searchQuery?: string;
}

/**
 * UserManagement Component
 * 
 * Displays and manages system users with filtering, sorting, and CRUD operations.
 * Provides both grid and table views with comprehensive user management functionality.
 */
export function UserManagement({ searchQuery = '' }: UserManagementProps) {
    const [viewMode, setViewMode] = useState<'grid' | 'table'>('grid');
    const [showEditDialog, setShowEditDialog] = useState(false);
    const [showPasswordDialog, setShowPasswordDialog] = useState(false);
    const [selectedUser, setSelectedUser] = useState<User | null>(null);
    const [filters, setFilters] = useState<UserFilters>({
        query: searchQuery,
        role: 'all',
        status: undefined,
        department: 'all',
        sortBy: 'name',
        sortOrder: 'asc',
    });

    const { users, filterUsers, updateUser, deleteUser, resetPassword, loading } = useUsers();
    const { roles } = useRoles();
    const { hasPermission } = useAuth();

    // Update filters when search query changes
    React.useEffect(() => {
        setFilters(prev => ({ ...prev, query: searchQuery }));
    }, [searchQuery]);

    // Filter and sort users
    const filteredUsers = useMemo(() => {
        return filterUsers(filters);
    }, [users, filters, filterUsers]);

    // Get unique departments for filter
    const departments = useMemo(() => {
        const uniqueDepartments = [...new Set(users.map(u => u.department).filter(Boolean))];
        return ['All', ...uniqueDepartments];
    }, [users]);

    const handleFilterChange = (key: keyof UserFilters, value: any) => {
        setFilters(prev => ({
            ...prev,
            [key]: value,
        }));
    };

    const toggleSortOrder = () => {
        setFilters(prev => ({
            ...prev,
            sortOrder: prev.sortOrder === 'asc' ? 'desc' : 'asc',
        }));
    };

    const clearFilters = () => {
        setFilters({
            query: searchQuery,
            role: 'all',
            status: undefined,
            department: 'all',
            sortBy: 'name',
            sortOrder: 'asc',
        });
    };

    const handleEditUser = (user: User) => {
        if (hasPermission('users.manage')) {
            setSelectedUser(user);
            setShowEditDialog(true);
        }
    };

    const handleChangePassword = (user: User) => {
        if (hasPermission('users.manage')) {
            setSelectedUser(user);
            setShowPasswordDialog(true);
        }
    };

    const handleResetPassword = async (user: User) => {
        if (hasPermission('users.manage')) {
            const result = await resetPassword(user.id);
            if (result.success) {
                alert(`Password reset successfully. Temporary password: ${result.tempPassword}`);
            }
        }
    };

    const handleDeleteUser = async (user: User) => {
        if (hasPermission('users.manage') && confirm(`Are you sure you want to delete ${user.name}?`)) {
            await deleteUser(user.id);
        }
    };

    const handleToggleStatus = async (user: User) => {
        if (hasPermission('users.manage')) {
            const newStatus: UserStatus = user.status === 'active' ? 'inactive' : 'active';
            await updateUser(user.id, { status: newStatus });
        }
    };



    return (
        <div className="h-full flex flex-col">
            {/* Filters and Controls */}
            <div className="border-b p-4 space-y-4">
                {/* Search and View Toggle */}
                <div className="flex items-center gap-4">
                    <div className="relative flex-1">
                        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                        <Input
                            placeholder="Search users by name, email, role..."
                            value={filters.query || ''}
                            onChange={(e) => handleFilterChange('query', e.target.value)}
                            className="pl-9"
                        />
                    </div>
                    <div className="flex items-center gap-2">
                        <Button
                            variant={viewMode === 'grid' ? 'default' : 'outline'}
                            size="sm"
                            onClick={() => setViewMode('grid')}
                        >
                            <Grid3X3 className="h-4 w-4" />
                        </Button>
                        <Button
                            variant={viewMode === 'table' ? 'default' : 'outline'}
                            size="sm"
                            onClick={() => setViewMode('table')}
                        >
                            <List className="h-4 w-4" />
                        </Button>
                    </div>
                </div>

                {/* Advanced Filters */}
                <div className="flex items-center gap-4 flex-wrap">
                    <div className="flex items-center gap-2">
                        <Filter className="h-4 w-4 text-muted-foreground" />
                        <span className="text-sm text-muted-foreground">Filters:</span>
                    </div>

                    <Select value={filters.role || 'all'} onValueChange={(value) => handleFilterChange('role', value === 'all' ? undefined : value)}>
                        <SelectTrigger className="w-32">
                            <SelectValue placeholder="All Roles" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Roles</SelectItem>
                            {roles.map(role => (
                                <SelectItem key={role.id} value={role.id}>
                                    {role.name}
                                </SelectItem>
                            ))}
                        </SelectContent>
                    </Select>

                    <Select value={filters.status || 'all'} onValueChange={(value) => handleFilterChange('status', value === 'all' ? undefined : value as UserStatus)}>
                        <SelectTrigger className="w-32">
                            <SelectValue placeholder="All Status" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Status</SelectItem>
                            <SelectItem value="active">Active</SelectItem>
                            <SelectItem value="inactive">Inactive</SelectItem>
                            <SelectItem value="suspended">Suspended</SelectItem>
                            <SelectItem value="pending">Pending</SelectItem>
                        </SelectContent>
                    </Select>

                    <Select value={filters.department || 'all'} onValueChange={(value) => handleFilterChange('department', value === 'all' ? undefined : value)}>
                        <SelectTrigger className="w-36">
                            <SelectValue placeholder="All Departments" />
                        </SelectTrigger>
                        <SelectContent>
                            {departments.map(dept => (
                                <SelectItem key={dept || ''} value={dept?.toLowerCase() || ''}>
                                    {dept}
                                </SelectItem>
                            ))}
                        </SelectContent>
                    </Select>

                    <Select value={filters.sortBy || 'name'} onValueChange={(value) => handleFilterChange('sortBy', value)}>
                        <SelectTrigger className="w-32">
                            <SelectValue placeholder="Sort by" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="name">Name</SelectItem>
                            <SelectItem value="email">Email</SelectItem>
                            <SelectItem value="role">Role</SelectItem>
                            <SelectItem value="status">Status</SelectItem>
                            <SelectItem value="lastLoginAt">Last Login</SelectItem>
                            <SelectItem value="createdAt">Created</SelectItem>
                        </SelectContent>
                    </Select>

                    <Button variant="outline" size="sm" onClick={toggleSortOrder}>
                        {filters.sortOrder === 'asc' ? <SortAsc className="h-4 w-4" /> : <SortDesc className="h-4 w-4" />}
                    </Button>

                    <Button variant="outline" size="sm" onClick={clearFilters}>
                        Clear Filters
                    </Button>
                </div>

                {/* Results Summary */}
                <div className="flex items-center justify-between text-sm text-muted-foreground">
                    <span>
                        Showing {filteredUsers.length} of {users.length} users
                    </span>
                    {filters.query && (
                        <span>
                            Search results for "{filters.query}"
                        </span>
                    )}
                </div>
            </div>

            {/* User Display */}
            <div className="flex-1 overflow-auto">
                {loading ? (
                    <div className="flex items-center justify-center h-full">
                        <div className="text-center">
                            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
                            <p className="text-sm text-muted-foreground">Loading users...</p>
                        </div>
                    </div>
                ) : filteredUsers.length === 0 ? (
                    <div className="flex items-center justify-center h-full">
                        <div className="text-center">
                            <UserX className="h-12 w-12 text-muted-foreground mx-auto mb-4" />
                            <h3 className="text-lg font-semibold mb-2">No users found</h3>
                            <p className="text-muted-foreground mb-4">
                                {filters.query ? 'Try adjusting your search criteria' : 'No users match the current filters'}
                            </p>
                        </div>
                    </div>
                ) : viewMode === 'grid' ? (
                    <div className="p-4">
                        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
                            {filteredUsers.map(user => (
                                <UserCard
                                    key={user.id}
                                    user={user}
                                    onEdit={() => handleEditUser(user)}
                                    onChangePassword={() => handleChangePassword(user)}
                                    onResetPassword={() => handleResetPassword(user)}
                                    onDelete={() => handleDeleteUser(user)}
                                    onToggleStatus={() => handleToggleStatus(user)}
                                />
                            ))}
                        </div>
                    </div>
                ) : (
                    <UserTable
                        users={filteredUsers}
                        onEdit={handleEditUser}
                        onChangePassword={handleChangePassword}
                        onResetPassword={handleResetPassword}
                        onDelete={handleDeleteUser}
                        onToggleStatus={handleToggleStatus}
                    />
                )}
            </div>

            {/* Dialogs */}
            <EditUserDialog
                isOpen={showEditDialog}
                user={selectedUser}
                onClose={() => {
                    setShowEditDialog(false);
                    setSelectedUser(null);
                }}
            />

            <ChangePasswordDialog
                isOpen={showPasswordDialog}
                user={selectedUser}
                onClose={() => {
                    setShowPasswordDialog(false);
                    setSelectedUser(null);
                }}
            />
        </div>
    );
} 