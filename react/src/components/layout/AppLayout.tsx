import { type ReactNode, useState } from 'react';
import { useLocation, Link } from 'react-router-dom';
import {
    Sidebar,
    SidebarHeader,
    SidebarContent,
    SidebarFooter,
    SidebarNav,
    SidebarNavItem,
} from '@/components/ui/sidebar';
import { AppNavbar } from './AppNavbar';
import {
    ShoppingCart,
    Package,
    Users,
    Store,
    BarChart3,
    Calendar,
    Settings
} from 'lucide-react';

interface AppLayoutProps {
    children: ReactNode;
}

export function AppLayout({ children }: AppLayoutProps) {
    const location = useLocation();
    const [searchQuery, setSearchQuery] = useState('');

    const navigationItems = [
        {
            title: 'Point of Sale',
            href: '/',
            icon: ShoppingCart,
        },
        {
            title: 'Inventory',
            href: '/inventory',
            icon: Package,
        },
        {
            title: 'Payment Plans',
            href: '/payment-plans',
            icon: Calendar,
        },
        {
            title: 'Reports',
            href: '/reports',
            icon: BarChart3,
        },
        {
            title: 'Administration',
            href: '/admin',
            icon: Users,
        },
        {
            title: 'Settings',
            href: '/settings',
            icon: Settings,
        },
    ];

    const handleSearchChange = (query: string) => {
        setSearchQuery(query);
    };

    return (
        <div className="flex h-screen bg-background">
            <Sidebar>
                <SidebarHeader>
                    <div className="flex items-center gap-2">
                        <Store className="h-6 w-6" />
                        <span className="font-bold text-lg">RetailUp</span>
                    </div>
                </SidebarHeader>

                <SidebarContent>
                    <SidebarNav>
                        {navigationItems.map((item) => {
                            const Icon = item.icon;
                            const isActive = location.pathname === item.href ||
                                (item.href !== '/' && location.pathname.startsWith(item.href));

                            return (
                                <SidebarNavItem key={item.href} asChild active={isActive}>
                                    <Link to={item.href} className="flex items-center gap-3">
                                        <Icon className="h-4 w-4" />
                                        <span>{item.title}</span>
                                    </Link>
                                </SidebarNavItem>
                            );
                        })}
                    </SidebarNav>
                </SidebarContent>

                <SidebarFooter>
                    <div className="text-xs text-muted-foreground">
                        RetailUp v1.0.0
                    </div>
                </SidebarFooter>
            </Sidebar>

            <div className="flex-1 flex flex-col overflow-hidden">
                <AppNavbar onSearchChange={handleSearchChange} />
                <main className="flex-1 overflow-auto">
                    {children}
                </main>
            </div>
        </div>
    );
} 