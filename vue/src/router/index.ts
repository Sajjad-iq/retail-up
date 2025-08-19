import type { RouteRecordRaw } from 'vue-router'

// Route metadata interface for authentication
export interface RouteMeta {
    requiresAuth: boolean
    title?: string
    icon?: string
}

// Simple route type with custom meta
export type AppRouteRecordRaw = RouteRecordRaw & {
    meta?: RouteMeta
}

// Route definitions with simple authentication check
export const routes: AppRouteRecordRaw[] = [
    {
        path: '/',
        redirect: '/dashboard'
    },
    {
        path: '/auth',
        name: 'Auth',
        component: () => import('@/pages/auth/page.vue'),
        meta: {
            requiresAuth: false,
            title: 'Authentication'
        }
    },
    {
        path: '/organization',
        name: 'Organization',
        component: () => import('@/pages/organization/page.vue'),
        meta: {
            requiresAuth: true,
            title: 'Organization Setup'
        }
    },
    {
        path: '/organization-selection',
        name: 'OrganizationSelection',
        component: () => import('@/pages/organization-selection/page.vue'),
        meta: {
            requiresAuth: true,
            title: 'Select Organization'
        }
    },
    {
        path: '/dashboard',
        name: 'Dashboard',
        component: () => import('@/pages/dashboard/page.vue'),
        meta: {
            requiresAuth: true,
            title: 'Dashboard',
            icon: 'dashboard'
        }
    }
]

export default routes
