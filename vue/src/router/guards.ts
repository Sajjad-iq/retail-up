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
        console.log('Route requires authentication, redirecting to auth')
        next({ name: 'Auth' })
        return
    }

    // If user is authenticated and has type USER, redirect to dashboard
    if (authStore.isAuthenticated && authStore.user?.accountType === AccountType.USER) {
        if (to.name !== 'Dashboard') {
            console.log('User type USER, redirecting to dashboard')
            next({ name: 'Dashboard' })
            return
        }
    }

    // Check if user needs to set up organization
    if (authStore.isAuthenticated && !authStore.organization && to.name !== 'Organization') {
        console.log('User authenticated but no organization, redirecting to organization setup')
        next({ name: 'Organization' })
        return
    }

    // Allow navigation
    next()
}

export function setupRouterGuards(router: any) {
    router.beforeEach(authGuard)
}
