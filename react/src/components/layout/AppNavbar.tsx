import { useState } from 'react';
import { useLocation } from 'react-router-dom';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Button } from '@/components/ui/button';
import {
    DropdownMenu,
    DropdownMenuContent,
    DropdownMenuItem,
    DropdownMenuLabel,
    DropdownMenuSeparator,
    DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu';
import {
    Store,
    Package,
    Shield,
    Search,
    User,
    Clock,
    AlertTriangle,
    Settings,
    LogOut,
    UserCircle,
    Bell,
    HelpCircle
} from 'lucide-react';

// Import hooks for different apps
import { useTransactions } from '@/apps/pos/hooks/use-pos';
import { useInventoryItems, useInventoryAlerts } from '@/apps/inventory/hooks/use-inventory';
import { useAuth, useAuthAnalytics } from '@/apps/auth/hooks/use-auth';

interface AppNavbarProps {
    onSearchChange?: (query: string) => void;
}

export function AppNavbar({ onSearchChange }: AppNavbarProps) {
    const location = useLocation();
    const [searchQuery, setSearchQuery] = useState('');

    const { currentUser, userDisplayName, userInitials, logout, isAuthenticated } = useAuth();

    const handleSearchChange = (value: string) => {
        setSearchQuery(value);
        onSearchChange?.(value);
    };

    const handleLogout = () => {
        logout();
    };



    // Get page title based on route
    const getPageTitle = () => {
        if (location.pathname === '/') return 'Point of Sale';
        if (location.pathname.startsWith('/inventory')) return 'Inventory Management';
        if (location.pathname.startsWith('/admin')) return 'Administration';
        return 'RetailUp';
    };

    return (
        <header className="border-b bg-card h-[73px] flex items-center">
            <div className="flex items-center justify-between p-4 w-full">
                {/* Left Section - Title */}
                <div className="flex items-center gap-4">
                    <div className="flex items-center gap-2">
                        <Store className="h-6 w-6 text-primary" />
                        <h1 className="text-xl font-bold text-foreground">RetailUp</h1>
                    </div>
                    <div className="hidden sm:block text-muted-foreground">
                        <span className="text-sm">•</span>
                        <span className="ml-2 text-sm">{getPageTitle()}</span>
                    </div>
                </div>

                {/* Center Section - Search */}
                <div className="hidden md:flex items-center gap-4 flex-1 max-w-md mx-8">
                    <div className="relative flex-1">
                        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                        <Input
                            placeholder="Search..."
                            value={searchQuery}
                            onChange={(e) => handleSearchChange(e.target.value)}
                            className="pl-9"
                        />
                    </div>
                </div>

                {/* Right Section - Actions and User */}
                <div className="flex items-center gap-3">
                    {/* Notifications */}
                    <Button variant="ghost" size="sm" className="relative">
                        <Bell className="h-4 w-4" />
                        <Badge
                            variant="destructive"
                            className="absolute -top-1 -right-1 h-4 w-4 p-0 text-xs flex items-center justify-center"
                        >
                            3
                        </Badge>
                    </Button>

                    {/* Help */}
                    <Button variant="ghost" size="sm">
                        <HelpCircle className="h-4 w-4" />
                    </Button>

                    {/* User Avatar with Dropdown */}
                    <DropdownMenu>
                        <DropdownMenuTrigger asChild>
                            <Button variant="ghost" className="relative h-10 w-10 rounded-full">
                                <Avatar className="h-8 w-8">
                                    <AvatarFallback className="bg-primary text-primary-foreground text-sm font-medium">
                                        {isAuthenticated && userInitials ? userInitials : <User className="h-4 w-4" />}
                                    </AvatarFallback>
                                </Avatar>
                            </Button>
                        </DropdownMenuTrigger>
                        <DropdownMenuContent className="w-56" align="end" forceMount>
                            <DropdownMenuLabel className="font-normal">
                                <div className="flex flex-col space-y-1">
                                    <p className="text-sm font-medium leading-none">
                                        {isAuthenticated && userDisplayName ? userDisplayName : 'Guest User'}
                                    </p>
                                    <p className="text-xs leading-none text-muted-foreground">
                                        {isAuthenticated && currentUser?.permissions ?
                                            `${currentUser.permissions.length} permissions` :
                                            'No permissions assigned'
                                        }
                                    </p>
                                </div>
                            </DropdownMenuLabel>
                            <DropdownMenuSeparator />

                            <DropdownMenuItem>
                                <UserCircle className="mr-2 h-4 w-4" />
                                <span>Profile Settings</span>
                            </DropdownMenuItem>

                            <DropdownMenuItem>
                                <Settings className="mr-2 h-4 w-4" />
                                <span>App Settings</span>
                            </DropdownMenuItem>

                            {isAuthenticated && currentUser?.role?.name === 'Admin' && (
                                <DropdownMenuItem>
                                    <Shield className="mr-2 h-4 w-4" />
                                    <span>System Settings</span>
                                </DropdownMenuItem>
                            )}

                            <DropdownMenuSeparator />

                            <DropdownMenuItem onClick={handleLogout}>
                                <LogOut className="mr-2 h-4 w-4" />
                                <span>Log out</span>
                            </DropdownMenuItem>
                        </DropdownMenuContent>
                    </DropdownMenu>
                </div>
            </div>
        </header>
    );
} 