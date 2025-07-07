import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
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
    Phone,
    Building,
    User,
    Shield
} from 'lucide-react';

import { useAuth, useAuthFormatters } from '../../hooks/use-auth';
import type { User as UserType } from '../../types/auth';

/**
 * UserCard component props
 */
interface UserCardProps {
    /** User to display */
    user: UserType;
    /** Callback when user edit is requested */
    onEdit?: () => void;
    /** Callback when password change is requested */
    onChangePassword?: () => void;
    /** Callback when password reset is requested */
    onResetPassword?: () => void;
    /** Callback when user deletion is requested */
    onDelete?: () => void;
    /** Callback when user status toggle is requested */
    onToggleStatus?: () => void;
}

/**
 * UserCard Component
 * 
 * Displays user information in a card format for grid view.
 * Provides quick access to user management actions and status information.
 */
export function UserCard({
    user,
    onEdit,
    onChangePassword,
    onResetPassword,
    onDelete,
    onToggleStatus
}: UserCardProps) {
    const { hasPermission, currentUser } = useAuth();
    const { formatUserStatus, getUserStatusColor, getUserInitials, formatLastLogin, formatDate } = useAuthFormatters();

    /**
     * Get status indicator for card display
     */
    const getStatusIndicator = () => {
        switch (user.status) {
            case 'active':
                return <CheckCircle className="h-4 w-4 text-green-500" />;
            case 'inactive':
                return <XCircle className="h-4 w-4 text-gray-500" />;
            case 'suspended':
                return <AlertTriangle className="h-4 w-4 text-red-500" />;
            case 'pending':
                return <Clock className="h-4 w-4 text-yellow-500" />;
        }
    };

    /**
     * Get status badge for user status
     */
    const getStatusBadge = () => {
        const variant = user.status === 'active' ? 'default' :
            user.status === 'suspended' ? 'destructive' :
                user.status === 'pending' ? 'secondary' : 'outline';

        return (
            <Badge variant={variant as any} className="text-xs">
                {formatUserStatus(user.status)}
            </Badge>
        );
    };

    const canManageUser = hasPermission('users.manage') && user.id !== currentUser?.id;

    return (
        <Card className="hover:shadow-md transition-shadow">
            <CardHeader className="pb-3">
                <div className="flex items-start justify-between">
                    <div className="flex items-center gap-3">
                        <Avatar className="h-12 w-12">
                            <AvatarFallback className="text-sm">
                                {getUserInitials(user)}
                            </AvatarFallback>
                        </Avatar>
                        <div className="flex-1">
                            <CardTitle className="text-base">{user.name}</CardTitle>
                            <CardDescription className="text-sm">
                                {user.email}
                            </CardDescription>
                        </div>
                    </div>

                    {/* Status and Actions */}
                    <div className="flex items-center gap-2">
                        {getStatusIndicator()}
                        {user.mustChangePassword && (
                            <span title="Must change password">
                                <Key className="h-4 w-4 text-orange-500" />
                            </span>
                        )}

                        {hasPermission('users.manage') && (
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

                                    <DropdownMenuItem onClick={onEdit}>
                                        <Edit className="mr-2 h-4 w-4" />
                                        Edit User
                                    </DropdownMenuItem>

                                    <DropdownMenuItem onClick={onChangePassword}>
                                        <Key className="mr-2 h-4 w-4" />
                                        Change Password
                                    </DropdownMenuItem>

                                    <DropdownMenuItem onClick={onResetPassword}>
                                        <RotateCcw className="mr-2 h-4 w-4" />
                                        Reset Password
                                    </DropdownMenuItem>

                                    <DropdownMenuSeparator />

                                    <DropdownMenuItem onClick={onToggleStatus}>
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

                                    {canManageUser && (
                                        <>
                                            <DropdownMenuSeparator />
                                            <DropdownMenuItem
                                                onClick={onDelete}
                                                className="text-red-600 dark:text-red-400"
                                            >
                                                <Trash2 className="mr-2 h-4 w-4" />
                                                Delete User
                                            </DropdownMenuItem>
                                        </>
                                    )}
                                </DropdownMenuContent>
                            </DropdownMenu>
                        )}
                    </div>
                </div>
            </CardHeader>

            <CardContent className="space-y-4">
                {/* Role */}
                <div className="flex items-center gap-2">
                    <Shield className="h-3 w-3 text-muted-foreground" />
                    <span className="text-sm">
                        {user.permissions?.length || 0} permissions
                    </span>
                </div>

                {/* Status */}
                <div className="flex items-center justify-between">
                    {getStatusBadge()}
                </div>

                {/* Contact Information */}
                <div className="space-y-2">
                    {user.phone && (
                        <div className="flex items-center gap-2 text-sm text-muted-foreground">
                            <Phone className="h-3 w-3" />
                            {user.phone}
                        </div>
                    )}

                    {user.department && (
                        <div className="flex items-center gap-2 text-sm text-muted-foreground">
                            <Building className="h-3 w-3" />
                            {user.department}
                        </div>
                    )}

                    {user.employeeId && (
                        <div className="flex items-center gap-2 text-sm text-muted-foreground">
                            <User className="h-3 w-3" />
                            <code className="text-xs bg-muted px-1 py-0.5 rounded">
                                {user.employeeId}
                            </code>
                        </div>
                    )}
                </div>

                {/* Login Information */}
                <div className="pt-2 border-t">
                    <div className="space-y-1">
                        <div className="flex items-center justify-between text-xs text-muted-foreground">
                            <span>Last Login:</span>
                            <span>{formatLastLogin(user.lastLoginAt)}</span>
                        </div>
                        <div className="flex items-center justify-between text-xs text-muted-foreground">
                            <span>Created:</span>
                            <span>{formatDate(user.createdAt)}</span>
                        </div>
                    </div>
                </div>

                {/* Quick Actions (if user has permissions) */}
                {hasPermission('users.manage') && (
                    <div className="flex gap-2">
                        <Button variant="outline" size="sm" className="flex-1" onClick={onEdit}>
                            <Edit className="h-3 w-3 mr-1" />
                            Edit
                        </Button>
                        <Button variant="outline" size="sm" className="flex-1" onClick={onChangePassword}>
                            <Key className="h-3 w-3 mr-1" />
                            Password
                        </Button>
                    </div>
                )}
            </CardContent>
        </Card>
    );
} 