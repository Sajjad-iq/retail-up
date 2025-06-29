import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuLabel,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu';
import {
    MoreHorizontal,
    Edit,
    Key,
    RotateCcw,
    Trash2,
    UserCheck,
    UserX,
    CheckCircle,
    XCircle,
    AlertTriangle,
    Clock,
    Mail,
    Phone,
    Building,
    Calendar
} from 'lucide-react';

import { useAuth, useAuthFormatters } from '../../hooks/use-auth';
import type { User } from '../../types/auth';

/**
 * UserTable component props
 */
interface UserTableProps {
    /** Array of users to display */
    users: User[];
    /** Callback when user edit is requested */
    onEdit?: (user: User) => void;
    /** Callback when password change is requested */
    onChangePassword?: (user: User) => void;
    /** Callback when password reset is requested */
    onResetPassword?: (user: User) => void;
    /** Callback when user deletion is requested */
    onDelete?: (user: User) => void;
    /** Callback when user status toggle is requested */
    onToggleStatus?: (user: User) => void;
}

/**
 * UserTable Component
 * 
 * Displays users in a table format with sortable columns and action buttons.
 * Provides detailed view of user information including role, status, and last login.
 */
export function UserTable({
    users,
    onEdit,
    onChangePassword,
    onResetPassword,
    onDelete,
    onToggleStatus
}: UserTableProps) {
    const { hasPermission, currentUser } = useAuth();
    const { formatUserStatus, getUserInitials, formatLastLogin, formatDate } = useAuthFormatters();

    /**
     * Get status indicator for table display
     */
    const getStatusIndicator = (user: User) => {
        switch (user.status) {
            case 'active':
                return <span title="Active"><CheckCircle className="h-4 w-4 text-green-500" /></span>;
            case 'inactive':
                return <span title="Inactive"><XCircle className="h-4 w-4 text-gray-500" /></span>;
            case 'suspended':
                return <span title="Suspended"><AlertTriangle className="h-4 w-4 text-red-500" /></span>;
            case 'pending':
                return <span title="Pending"><Clock className="h-4 w-4 text-yellow-500" /></span>;
        }
    };

    /**
     * Get status badge for user status
     */
    const getStatusBadge = (status: string) => {
        const variant = status === 'active' ? 'default' :
            status === 'suspended' ? 'destructive' :
                status === 'pending' ? 'secondary' : 'outline';

        return (
            <Badge variant={variant as any} className="text-xs">
                {formatUserStatus(status as any)}
            </Badge>
        );
    };

    /**
     * Get role badge
     */
    const getRoleBadge = (user: User) => {
        const roleColor = user.role.color || '#6B7280';
        return (
            <Badge
                variant="outline"
                className="text-xs"
                style={{ borderColor: roleColor, color: roleColor }}
            >
                {user.role.name}
            </Badge>
        );
    };

    const handleEdit = (user: User) => {
        onEdit?.(user);
    };

    const handleChangePassword = (user: User) => {
        onChangePassword?.(user);
    };

    const handleResetPassword = (user: User) => {
        onResetPassword?.(user);
    };

    const handleDelete = (user: User) => {
        onDelete?.(user);
    };

    const handleToggleStatus = (user: User) => {
        onToggleStatus?.(user);
    };

    const canManageUser = (user: User) => {
        return hasPermission('users.manage') && user.id !== currentUser?.id;
    };

    return (
        <div className="rounded-md border">
            <Table>
                <TableHeader>
                    <TableRow>
                        <TableHead className="w-[30px]">Status</TableHead>
                        <TableHead className="min-w-[250px]">User</TableHead>
                        <TableHead>Role</TableHead>
                        <TableHead>Department</TableHead>
                        <TableHead>Employee ID</TableHead>
                        <TableHead>Contact</TableHead>
                        <TableHead>Status</TableHead>
                        <TableHead>Last Login</TableHead>
                        <TableHead>Created</TableHead>
                        <TableHead className="text-right">Actions</TableHead>
                    </TableRow>
                </TableHeader>
                <TableBody>
                    {users.map((user) => (
                        <TableRow key={user.id}>
                            {/* Status Indicator */}
                            <TableCell>
                                <div className="flex items-center gap-2">
                                    {getStatusIndicator(user)}
                                    {user.mustChangePassword && (
                                        <span title="Must change password">
                                            <Key className="h-3 w-3 text-orange-500" />
                                        </span>
                                    )}
                                </div>
                            </TableCell>

                            {/* User Info */}
                            <TableCell>
                                <div className="flex items-center gap-3">
                                    <Avatar className="h-8 w-8">
                                        <AvatarFallback className="text-xs">
                                            {getUserInitials(user)}
                                        </AvatarFallback>
                                    </Avatar>
                                    <div>
                                        <div className="font-medium">{user.name}</div>
                                        <div className="text-sm text-muted-foreground flex items-center gap-1">
                                            <Mail className="h-3 w-3" />
                                            {user.email}
                                        </div>
                                    </div>
                                </div>
                            </TableCell>

                            {/* Role */}
                            <TableCell>
                                {getRoleBadge(user)}
                            </TableCell>

                            {/* Department */}
                            <TableCell>
                                <div className="flex items-center gap-1 text-sm">
                                    <Building className="h-3 w-3 text-muted-foreground" />
                                    {user.department || 'Not assigned'}
                                </div>
                            </TableCell>

                            {/* Employee ID */}
                            <TableCell>
                                <code className="text-xs bg-muted px-1 py-0.5 rounded">
                                    {user.employeeId || 'N/A'}
                                </code>
                            </TableCell>

                            {/* Contact */}
                            <TableCell>
                                {user.phone ? (
                                    <div className="flex items-center gap-1 text-sm">
                                        <Phone className="h-3 w-3 text-muted-foreground" />
                                        {user.phone}
                                    </div>
                                ) : (
                                    <span className="text-muted-foreground text-sm">No phone</span>
                                )}
                            </TableCell>

                            {/* Status */}
                            <TableCell>
                                {getStatusBadge(user.status)}
                            </TableCell>

                            {/* Last Login */}
                            <TableCell>
                                <div className="text-sm">
                                    {formatLastLogin(user.lastLoginAt)}
                                </div>
                            </TableCell>

                            {/* Created */}
                            <TableCell>
                                <div className="flex items-center gap-1 text-sm text-muted-foreground">
                                    <Calendar className="h-3 w-3" />
                                    {formatDate(user.createdAt)}
                                </div>
                            </TableCell>

                            {/* Actions */}
                            <TableCell className="text-right">
                                <DropdownMenu>
                                    <DropdownMenuTrigger asChild>
                                        <Button variant="ghost" className="h-8 w-8 p-0">
                                            <span className="sr-only">Open menu</span>
                                            <MoreHorizontal className="h-4 w-4" />
                                        </Button>
                                    </DropdownMenuTrigger>
                                    <DropdownMenuContent align="end">
                                        <DropdownMenuLabel>Actions</DropdownMenuLabel>
                                        <DropdownMenuSeparator />

                                        {hasPermission('users.manage') && (
                                            <>
                                                <DropdownMenuItem onClick={() => handleEdit(user)}>
                                                    <Edit className="mr-2 h-4 w-4" />
                                                    Edit User
                                                </DropdownMenuItem>

                                                <DropdownMenuItem onClick={() => handleChangePassword(user)}>
                                                    <Key className="mr-2 h-4 w-4" />
                                                    Change Password
                                                </DropdownMenuItem>

                                                <DropdownMenuItem onClick={() => handleResetPassword(user)}>
                                                    <RotateCcw className="mr-2 h-4 w-4" />
                                                    Reset Password
                                                </DropdownMenuItem>

                                                <DropdownMenuSeparator />

                                                <DropdownMenuItem onClick={() => handleToggleStatus(user)}>
                                                    {user.status === 'active' ? (
                                                        <>
                                                            <UserX className="mr-2 h-4 w-4" />
                                                            Deactivate
                                                        </>
                                                    ) : (
                                                        <>
                                                            <UserCheck className="mr-2 h-4 w-4" />
                                                            Activate
                                                        </>
                                                    )}
                                                </DropdownMenuItem>

                                                {canManageUser(user) && (
                                                    <>
                                                        <DropdownMenuSeparator />
                                                        <DropdownMenuItem
                                                            onClick={() => handleDelete(user)}
                                                            className="text-red-600 dark:text-red-400"
                                                        >
                                                            <Trash2 className="mr-2 h-4 w-4" />
                                                            Delete User
                                                        </DropdownMenuItem>
                                                    </>
                                                )}
                                            </>
                                        )}
                                    </DropdownMenuContent>
                                </DropdownMenu>
                            </TableCell>
                        </TableRow>
                    ))}
                </TableBody>
            </Table>
        </div>
    );
} 