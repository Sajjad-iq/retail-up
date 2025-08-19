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
        console.log('Auth store - setToken called with:', newToken)
        token.value = newToken
        // Only store token in localStorage
        if (newToken) {
            localStorage.setItem('token', newToken)
            console.log('Auth store - token saved to localStorage')
        }
        console.log('Auth store - current token value:', token.value)
        console.log('Auth store - localStorage token:', localStorage.getItem('token'))
    }

    const setOrganization = (newOrg: Organization | null) => {
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
        console.log('Auth store - initializeToken - stored token from localStorage:', storedToken)
        if (storedToken) {
            token.value = storedToken
            console.log('Auth store - initializeToken - token restored to store:', token.value)
        } else {
            console.log('Auth store - initializeToken - no token found in localStorage')
        }
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
        clearSession,
        initializeToken
    }
})
