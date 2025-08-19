import type { NavigationGuardNext, RouteLocationNormalized } from 'vue-router'
import { useAuthStore } from '@/stores/auth'
import { AccountType } from '@/types/global'

export async function authGuard(
    to: RouteLocationNormalized,
    from: RouteLocationNormalized,
    next: NavigationGuardNext
) {
    const authStore = useAuthStore()
    const requiresAuth = (to.meta as any)?.requiresAuth

    // Check if authentication is required
    if (requiresAuth && !authStore.isAuthenticated) {
        next({ name: 'Auth' })
        return
    }

    // If user is not authenticated, allow navigation to auth page
    if (!authStore.isAuthenticated) {
        next()
        return
    }

    // Check if user needs to set up organization first
    if (!authStore.organization && to.name !== 'Organization') {
        next({ name: 'Organization' })
        return
    }

    // User has organization, now check account type restrictions
    if (authStore.user?.accountType === AccountType.USER) {
        // For USER type, only allow dashboard and organization pages
        const allowedRoutes = ['Dashboard', 'Organization']
        if (!allowedRoutes.includes(to.name as string)) {
            console.log('User type USER, redirecting to dashboard')
            next({ name: 'Dashboard' })
            return
        }
    }

    // Allow navigation for all other cases
    next()
}

export function setupRouterGuards(router: any) {
    router.beforeEach(authGuard)
}
