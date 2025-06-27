import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import {
    Shield,
    ShieldCheck,
    Users,
    Settings,
    Search,
    Plus,
    Edit,
    Trash2,
    MoreHorizontal
} from 'lucide-react';

import { useRoles, useAuth } from '../../hooks/use-auth';
import type { Role } from '../../types/auth';

/**
 * RoleManagement component props
 */
interface RoleManagementProps {
    /** Search query from parent component */
    searchQuery?: string;
}

/**
 * RoleManagement Component
 * 
 * Displays and manages user roles with permissions.
 * Provides role creation, editing, and deletion functionality.
 */
export function RoleManagement({ searchQuery = '' }: RoleManagementProps) {
    const [localSearchQuery, setLocalSearchQuery] = useState(searchQuery);

    const { roles, rolesWithCounts, groupedPermissions, deleteRole, loading } = useRoles();
    const { hasPermission } = useAuth();

    // Filter roles based on search
    const filteredRoles = roles.filter(role =>
        role.name.toLowerCase().includes(localSearchQuery.toLowerCase()) ||
        role.description?.toLowerCase().includes(localSearchQuery.toLowerCase())
    );

    const handleDeleteRole = async (role: Role) => {
        if (hasPermission('users.roles') && !role.isSystem) {
            if (confirm(`Are you sure you want to delete the role "${role.name}"?`)) {
                await deleteRole(role.id);
            }
        }
    };

    return (
        <div className="h-full flex flex-col">
            {/* Header with Search */}
            <div className="border-b p-4">
                <div className="flex items-center gap-4">
                    <div className="relative flex-1">
                        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                        <Input
                            placeholder="Search roles..."
                            value={localSearchQuery}
                            onChange={(e) => setLocalSearchQuery(e.target.value)}
                            className="pl-9"
                        />
                    </div>
                </div>
            </div>

            {/* Roles Grid */}
            <div className="flex-1 overflow-auto p-4">
                {loading ? (
                    <div className="flex items-center justify-center h-full">
                        <div className="text-center">
                            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
                            <p className="text-sm text-muted-foreground">Loading roles...</p>
                        </div>
                    </div>
                ) : (
                    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                        {filteredRoles.map(role => {
                            const roleWithCounts = rolesWithCounts.find(r => r.id === role.id);
                            return (
                                <Card key={role.id} className="hover:shadow-md transition-shadow">
                                    <CardHeader className="pb-3">
                                        <div className="flex items-start justify-between">
                                            <div className="flex items-center gap-2">
                                                <Shield
                                                    className="h-5 w-5"
                                                    style={{ color: role.color || '#6B7280' }}
                                                />
                                                <div>
                                                    <CardTitle className="text-base">{role.name}</CardTitle>
                                                    {role.description && (
                                                        <CardDescription className="text-sm">
                                                            {role.description}
                                                        </CardDescription>
                                                    )}
                                                </div>
                                            </div>

                                            <div className="flex items-center gap-2">
                                                {role.isSystem && (
                                                    <Badge variant="secondary" className="text-xs">
                                                        System
                                                    </Badge>
                                                )}
                                            </div>
                                        </div>
                                    </CardHeader>

                                    <CardContent className="space-y-3">
                                        {/* User Count */}
                                        <div className="flex items-center justify-between text-sm">
                                            <div className="flex items-center gap-2">
                                                <Users className="h-4 w-4 text-muted-foreground" />
                                                <span>Users assigned:</span>
                                            </div>
                                            <Badge variant="outline">
                                                {roleWithCounts?.userCount || 0}
                                            </Badge>
                                        </div>

                                        {/* Permission Count */}
                                        <div className="flex items-center justify-between text-sm">
                                            <div className="flex items-center gap-2">
                                                <ShieldCheck className="h-4 w-4 text-muted-foreground" />
                                                <span>Permissions:</span>
                                            </div>
                                            <Badge variant="outline">
                                                {role.permissions.length}
                                            </Badge>
                                        </div>

                                        {/* Actions */}
                                        {hasPermission('users.roles') && (
                                            <div className="flex gap-2 pt-2">
                                                <Button variant="outline" size="sm" className="flex-1">
                                                    <Edit className="h-3 w-3 mr-1" />
                                                    Edit
                                                </Button>
                                                {!role.isSystem && (
                                                    <Button
                                                        variant="outline"
                                                        size="sm"
                                                        className="flex-1 text-red-600 hover:text-red-700"
                                                        onClick={() => handleDeleteRole(role)}
                                                    >
                                                        <Trash2 className="h-3 w-3 mr-1" />
                                                        Delete
                                                    </Button>
                                                )}
                                            </div>
                                        )}
                                    </CardContent>
                                </Card>
                            );
                        })}
                    </div>
                )}
            </div>
        </div>
    );
} 