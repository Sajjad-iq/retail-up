import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Activity, Clock, LogIn, LogOut, UserPlus, Edit } from 'lucide-react';

import { useUserActivities, useAuthFormatters } from '../../hooks/use-auth';

/**
 * UserActivities Component
 * 
 * Displays user activity logs and audit trail.
 * Shows recent actions performed by users in the system.
 */
export function UserActivities() {
    const { recentActivities, todaysActivities } = useUserActivities();
    const { formatDateTime, getUserInitials } = useAuthFormatters();

    const getActivityIcon = (action: string) => {
        switch (action) {
            case 'login':
                return <LogIn className="h-4 w-4 text-green-500" />;
            case 'logout':
                return <LogOut className="h-4 w-4 text-gray-500" />;
            case 'create_user':
                return <UserPlus className="h-4 w-4 text-blue-500" />;
            case 'update_user':
                return <Edit className="h-4 w-4 text-orange-500" />;
            default:
                return <Activity className="h-4 w-4 text-gray-500" />;
        }
    };

    const getActionLabel = (action: string) => {
        switch (action) {
            case 'login':
                return 'Logged in';
            case 'logout':
                return 'Logged out';
            case 'create_user':
                return 'Created user';
            case 'update_user':
                return 'Updated user';
            case 'delete_user':
                return 'Deleted user';

            case 'update_role':
                return 'Updated role';
            case 'delete_role':
                return 'Deleted role';
            case 'change_password':
                return 'Changed password';
            case 'reset_password':
                return 'Reset password';
            default:
                return action.replace('_', ' ');
        }
    };

    return (
        <div className="h-full flex flex-col p-4 space-y-4">
            {/* Today's Summary */}
            <Card>
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <Clock className="h-5 w-5" />
                        Today's Activity Summary
                    </CardTitle>
                    <CardDescription>
                        Activity overview for {new Date().toLocaleDateString()}
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                        <div className="text-center">
                            <div className="text-2xl font-bold text-green-600">
                                {todaysActivities.filter(a => a.action === 'login').length}
                            </div>
                            <div className="text-sm text-muted-foreground">Logins</div>
                        </div>
                        <div className="text-center">
                            <div className="text-2xl font-bold text-blue-600">
                                {todaysActivities.filter(a => a.action.includes('user')).length}
                            </div>
                            <div className="text-sm text-muted-foreground">User Actions</div>
                        </div>
                        <div className="text-center">
                            <div className="text-2xl font-bold text-purple-600">
                                {todaysActivities.filter(a => a.action.includes('role')).length}
                            </div>
                            <div className="text-sm text-muted-foreground">Role Actions</div>
                        </div>
                        <div className="text-center">
                            <div className="text-2xl font-bold text-orange-600">
                                {todaysActivities.length}
                            </div>
                            <div className="text-sm text-muted-foreground">Total Activities</div>
                        </div>
                    </div>
                </CardContent>
            </Card>

            {/* Recent Activities */}
            <Card className="flex-1">
                <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                        <Activity className="h-5 w-5" />
                        Recent Activities
                    </CardTitle>
                    <CardDescription>
                        Latest user actions and system events
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    {false ? (
                        <div className="text-center py-8">
                            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
                            <p className="text-sm text-muted-foreground">Loading activities...</p>
                        </div>
                    ) : recentActivities.length === 0 ? (
                        <div className="text-center py-8">
                            <Activity className="h-12 w-12 text-muted-foreground mx-auto mb-4" />
                            <p className="text-sm text-muted-foreground">No recent activities</p>
                        </div>
                    ) : (
                        <div className="space-y-4 max-h-96 overflow-y-auto">
                            {recentActivities.map((activity) => (
                                <div key={activity.id} className="flex items-start gap-3 p-3 border rounded-lg">
                                    <div className="flex-shrink-0">
                                        {getActivityIcon(activity.action)}
                                    </div>

                                    <div className="flex-1 min-w-0">
                                        <div className="flex items-center gap-2">
                                            <Avatar className="h-6 w-6">
                                                <AvatarFallback className="text-xs">
                                                    {getUserInitials(activity.user)}
                                                </AvatarFallback>
                                            </Avatar>
                                            <span className="font-medium text-sm">{activity.user.name}</span>
                                            <Badge variant="outline" className="text-xs">
                                                {activity.user.permissions?.length || 0} permissions
                                            </Badge>
                                        </div>

                                        <div className="mt-1">
                                            <span className="text-sm">
                                                {getActionLabel(activity.action)}
                                                {activity.resource && (
                                                    <span className="text-muted-foreground"> • {activity.resource}</span>
                                                )}
                                            </span>
                                        </div>

                                        {activity.details && (
                                            <div className="text-xs text-muted-foreground mt-1">
                                                {activity.details}
                                            </div>
                                        )}
                                    </div>

                                    <div className="flex-shrink-0 text-xs text-muted-foreground">
                                        {formatDateTime(activity.timestamp)}
                                    </div>
                                </div>
                            ))}
                        </div>
                    )}
                </CardContent>
            </Card>
        </div>
    );
} 