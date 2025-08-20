import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import type { User } from '@/types/global'
import type { OrganizationResponse } from '@/services/organizationService'

export const useAuthStore = defineStore('auth', () => {
    const user = ref<User | null>(null)
    const organization = ref<OrganizationResponse | null>(null)
    const token = ref<string | null>(null)
    const isAuthenticated = computed(() => !!token.value && !!user.value)
    const hasSelectedOrganization = computed(() => !!organization.value)

    // Setter methods
    const setUser = (newUser: User | null) => {
        user.value = newUser
    }

    const setToken = (newToken: string | null) => {
        token.value = newToken
        // Only store token in localStorage
        if (newToken) {
            localStorage.setItem('token', newToken)
        }
    }

    const setOrganization = (newOrg: OrganizationResponse | null) => {
        organization.value = newOrg
    }

    const clearSession = () => {
        user.value = null
        organization.value = null
        token.value = null
        localStorage.removeItem('token')
    }

    // Initialize token from localStorage
    const initializeToken = () => {
        const storedToken = localStorage.getItem('token')
        if (storedToken) {
            token.value = storedToken
        } else {
            console.error('Auth store - initializeToken - no token found in localStorage')
        }
    }

    return {
        // State
        user,
        organization,
        token,
        isAuthenticated,
        hasSelectedOrganization,

        // Setters
        setUser,
        setToken,
        setOrganization,
        clearSession,
        initializeToken
    }
})
