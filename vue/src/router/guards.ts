import type { NavigationGuardNext, RouteLocationNormalized } from 'vue-router'
import { useAuth } from '@/composables/useAuth'
import { AccountType } from '@/types/global'

export async function authGuard(
    to: RouteLocationNormalized,
    from: RouteLocationNormalized,
    next: NavigationGuardNext
) {
    const requiresAuth = (to.meta as any)?.requiresAuth
    const org = localStorage.getItem('selected_organization')
    const token = localStorage.getItem('token')

    // If route doesn't require auth, allow navigation
    if (!requiresAuth) {
        // But if user is authenticated and has no org, redirect to org selection
        if (token && !org && to.name !== 'OrganizationSelection') {
            next({ name: 'OrganizationSelection' })
            return
        }
        next()
        return
    }

    // If user is not authenticated, redirect to auth page
    if (!token) {
        next({ name: 'Auth' })
        return
    }

    // If user is authenticated but has no organization, redirect to organization selection
    if (!org && to.name !== 'OrganizationSelection') {
        next({ name: 'OrganizationSelection' })
        return
    }

    // Allow navigation for all other cases
    next()
}

export function setupRouterGuards(router: any) {
    router.beforeEach(authGuard)
}
