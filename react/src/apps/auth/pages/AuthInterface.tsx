import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
    Plus,
    Users,
    Activity,
    BarChart3,
    Settings,
    UserCheck,
    UserPlus,
    ShieldCheck,
    TrendingUp,
    TrendingDown,
    AlertCircle
} from 'lucide-react';

import { UserManagement } from '../components/users/UserManagement';
import { RoleManagement } from '../components/roles/RoleManagement';
import { UserActivities } from '../components/activities/UserActivities';
import { AuthAnalytics } from '../components/analytics/AuthAnalytics';
import { AddUserDialog } from '../components/forms/AddUserDialog';
import { AddRoleDialog } from '../components/forms/AddRoleDialog';
import { useAuth, useUsers, useRoles, useUserActivities, useAuthAnalytics } from '../hooks/use-auth';

/**
 * AuthInterface Component
 * 
 * Main authentication and user management interface following the inventory design pattern.
 * Multi-tab layout with user management, role management, activities, and analytics.
 */
export function AuthInterface() {
    const [activeTab, setActiveTab] = useState('users');
    const [showAddUserDialog, setShowAddUserDialog] = useState(false);
    const [showAddRoleDialog, setShowAddRoleDialog] = useState(false);

    const { currentUser, hasPermission, isAdmin, userDisplayName, userInitials } = useAuth();
    const { totalUsers, activeUserCount, usersRequiringPasswordChange } = useUsers();
    const { totalRoles, customRoles } = useRoles();
    const { todaysLoginCount, todaysUserManagementCount } = useUserActivities();
    const { activeUsers, loginsToday, newUsersThisMonth } = useAuthAnalytics();

    const handleAddUser = () => {
        if (hasPermission('users.manage')) {
            setShowAddUserDialog(true);
        }
    };

    const handleAddRole = () => {
        if (hasPermission('users.roles')) {
            setShowAddRoleDialog(true);
        }
    };

    const handleUserAdded = () => {
        setShowAddUserDialog(false);
    };

    const handleRoleAdded = () => {
        setShowAddRoleDialog(false);
    };

    // Check if user has access to this interface
    if (!hasPermission('users.view') && !isAdmin()) {
        return (
            <div className="h-full flex items-center justify-center bg-background">
                <div className="text-center">
                    <AlertCircle className="h-12 w-12 text-muted-foreground mx-auto mb-4" />
                    <h2 className="text-xl font-semibold mb-2">Access Denied</h2>
                    <p className="text-muted-foreground">
                        You don't have permission to access user management.
                    </p>
                </div>
            </div>
        );
    }

    return (
        <div className="h-full bg-background">
            {/* Main Content */}
            <div className="flex h-full">
                {/* Left Side - Main Content */}
                <div className="flex-1 flex flex-col">
                    <Tabs value={activeTab} onValueChange={setActiveTab} className="flex-1 flex flex-col">
                        {/* Tab Navigation */}
                        <div className="border-b px-4 py-3">
                            <TabsList className="h-10">
                                <TabsTrigger value="users" className="text-xs">
                                    <Users className="h-4 w-4 mr-1" />
                                    Users
                                    <Badge variant="secondary" className="ml-2 text-xs">
                                        {totalUsers}
                                    </Badge>
                                </TabsTrigger>
                                <TabsTrigger value="roles" className="text-xs">
                                    <ShieldCheck className="h-4 w-4 mr-1" />
                                    Roles
                                    <Badge variant="secondary" className="ml-2 text-xs">
                                        {totalRoles}
                                    </Badge>
                                </TabsTrigger>
                                <TabsTrigger value="activities" className="text-xs">
                                    <Activity className="h-4 w-4 mr-1" />
                                    Activities
                                </TabsTrigger>
                                <TabsTrigger value="analytics" className="text-xs">
                                    <BarChart3 className="h-4 w-4 mr-1" />
                                    Analytics
                                </TabsTrigger>
                            </TabsList>
                        </div>

                        {/* Tab Content */}
                        <div className="flex-1 overflow-hidden">
                            <TabsContent value="users" className="h-full m-0">
                                <UserManagement />
                            </TabsContent>

                            <TabsContent value="roles" className="h-full m-0">
                                <RoleManagement />
                            </TabsContent>

                            <TabsContent value="activities" className="h-full m-0">
                                <UserActivities />
                            </TabsContent>

                            <TabsContent value="analytics" className="h-full m-0">
                                <AuthAnalytics />
                            </TabsContent>
                        </div>
                    </Tabs>
                </div>

                {/* Right Sidebar - Quick Actions & Stats */}
                <div className="w-80 border-l bg-muted/20 flex flex-col">
                    {/* Quick Actions */}
                    <div className="border-b p-4">
                        <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                            <Settings className="h-4 w-4" />
                            Quick Actions
                        </h3>
                        <div className="space-y-2">
                            <Button
                                className="w-full justify-start"
                                size="sm"
                                onClick={handleAddUser}
                                disabled={!hasPermission('users.manage')}
                            >
                                <UserPlus className="mr-2 h-4 w-4" />
                                Add User
                            </Button>
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                size="sm"
                                onClick={handleAddRole}
                                disabled={!hasPermission('users.roles')}
                            >
                                <Plus className="mr-2 h-4 w-4" />
                                Add Role
                            </Button>
                        </div>
                    </div>

                    {/* Statistics */}
                    <div className="border-b p-4">
                        <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                            <BarChart3 className="h-4 w-4" />
                            Today's Overview
                        </h3>
                        <div className="space-y-3">
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <UserCheck className="h-4 w-4 text-green-500" />
                                    <span className="text-sm">Active Users</span>
                                </div>
                                <span className="text-sm font-medium">{activeUserCount}</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-blue-500" />
                                    <span className="text-sm">Logins Today</span>
                                </div>
                                <span className="text-sm font-medium">{loginsToday}</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <UserPlus className="h-4 w-4 text-purple-500" />
                                    <span className="text-sm">New This Month</span>
                                </div>
                                <span className="text-sm font-medium">{newUsersThisMonth}</span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <Activity className="h-4 w-4 text-orange-500" />
                                    <span className="text-sm">Management Actions</span>
                                </div>
                                <span className="text-sm font-medium">{todaysUserManagementCount}</span>
                            </div>
                        </div>
                    </div>

                    {/* Alerts */}
                    {usersRequiringPasswordChange.length > 0 && (
                        <div className="border-b p-4">
                            <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                                <AlertCircle className="h-4 w-4 text-orange-500" />
                                Attention Required
                            </h3>
                            <div className="space-y-2">
                                <div className="flex items-center justify-between p-2 bg-orange-50 dark:bg-orange-950/20 rounded-md">
                                    <div>
                                        <p className="text-xs font-medium text-orange-900 dark:text-orange-100">
                                            Password Changes Required
                                        </p>
                                        <p className="text-xs text-orange-700 dark:text-orange-300">
                                            {usersRequiringPasswordChange.length} users must change passwords
                                        </p>
                                    </div>
                                    <TrendingDown className="h-4 w-4 text-orange-500" />
                                </div>
                            </div>
                        </div>
                    )}

                    {/* System Info */}
                    <div className="mt-auto p-4 border-t">
                        <h3 className="text-sm font-medium mb-3">System Status</h3>
                        <div className="space-y-2 text-xs text-muted-foreground">
                            <div className="flex justify-between">
                                <span>Total Users:</span>
                                <span>{totalUsers}</span>
                            </div>
                            <div className="flex justify-between">
                                <span>System Roles:</span>
                                <span>{totalRoles - customRoles.length}</span>
                            </div>
                            <div className="flex justify-between">
                                <span>Custom Roles:</span>
                                <span>{customRoles.length}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Dialogs */}
            <AddUserDialog
                isOpen={showAddUserDialog}
                onClose={() => setShowAddUserDialog(false)}
                onSuccess={handleUserAdded}
            />

            <AddRoleDialog
                isOpen={showAddRoleDialog}
                onClose={() => setShowAddRoleDialog(false)}
                onSuccess={handleRoleAdded}
            />
        </div>
    );
} 