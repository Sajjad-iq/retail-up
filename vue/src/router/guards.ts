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

    // Check if user needs to select an organization first
    if (!authStore.hasSelectedOrganization && to.name !== 'OrganizationSelection' && AccountType.USER) {
        next({ name: 'OrganizationSelection' })
        return
    }

 

    // Allow navigation for all other cases
    next()
}

export function setupRouterGuards(router: any) {
    router.beforeEach(authGuard)
}
