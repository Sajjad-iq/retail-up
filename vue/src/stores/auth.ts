import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import type { User, Organization } from '@/types/global'

export const useAuthStore = defineStore('auth', () => {
    const user = ref<User | null>(null)
    const organization = ref<Organization | null>(null)
    const token = ref<string | null>(null)
    const isAuthenticated = computed(() => !!token.value && !!user.value)

    // Setter methods
    const setUser = (newUser: User | null) => {
        user.value = newUser
    }

    const setToken = (newToken: string | null) => {
        token.value = newToken
    }

    const setOrganization = (newOrg: Organization | null) => {
        organization.value = newOrg
    }

    const clearSession = () => {
        user.value = null
        organization.value = null
        token.value = null
    }

    return {
        // State
        user,
        organization,
        token,
        isAuthenticated,

        // Setters
        setUser,
        setToken,
        setOrganization,
        clearSession
    }
})
