import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import { authService } from '@/services/authService'
import { organizationService } from '@/services/organizationService'
import type { OrganizationResponse, OrganizationStatus } from '@/services/organizationService'

export interface User {
    id: string
    name: string
    email: string
    phone: string
}

export interface Organization {
    id: string
    name: string
    domain: string
    description?: string
    address?: string
    phone: string
    status: OrganizationStatus
    createdAt: string
    updatedAt: string
    createdBy: string
}

export const useAuthStore = defineStore('auth', () => {
    const user = ref<User | null>(null)
    const organization = ref<Organization | null>(null)
    const token = ref<string | null>(null)
    const isAuthenticated = computed(() => !!token.value)

    const login = async (emailOrPhone: string, password: string) => {
        try {
            const result = await authService.login({ emailOrPhone, password })

            if (result.success && result.data) {
                // Create user object from login response
                user.value = {
                    id: result.data.userId,
                    name: result.data.name,
                    email: result.data.email,
                    phone: result.data.phone
                }
                token.value = result.data.token

                // Store token in localStorage
                localStorage.setItem('token', result.data.token)
                localStorage.setItem('user', JSON.stringify(user.value))

                return { success: true, user: user.value }
            } else {
                return { success: false, error: result.error || 'Login failed' }
            }
        } catch (error) {
            console.error('Login failed:', error)
            return { success: false, error: 'Login failed' }
        }
    }

    const register = async (name: string, email: string, phone: string, password: string) => {
        try {
            const result = await authService.register({ name, email, phone, password })

            if (result.success && result.data) {
                // Create user object from register response
                user.value = {
                    id: result.data.userId,
                    name: result.data.name,
                    email: result.data.email,
                    phone: result.data.phone
                }
                token.value = result.data.token

                // Store token in localStorage
                localStorage.setItem('token', result.data.token)
                localStorage.setItem('user', JSON.stringify(user.value))

                return { success: true, user: user.value }
            } else {
                return { success: false, error: result.error || 'Registration failed' }
            }
        } catch (error) {
            console.error('Registration failed:', error)
            return { success: false, error: 'Registration failed' }
        }
    }

    const createOrganization = async (orgData: {
        name: string
        domain: string
        description?: string
        address?: string
        phone: string
        email?: string
    }) => {
        try {
            if (!token.value || !user.value) {
                return { success: false, error: 'No authentication token or user' }
            }

            const createRequest = {
                userId: user.value.id,
                ...orgData
            }

            const result = await organizationService.createOrganization(createRequest)

            if (result.success && result.data) {
                organization.value = result.data
                localStorage.setItem('organization', JSON.stringify(result.data))

                return { success: true, organization: result.data }
            } else {
                return { success: false, error: result.error || 'Organization creation failed' }
            }
        } catch (error) {
            console.error('Organization creation failed:', error)
            return { success: false, error: 'Organization creation failed' }
        }
    }

    const logout = async () => {
        try {
            await authService.logout()
        } catch (error) {
            console.error('Logout error:', error)
        } finally {
            user.value = null
            organization.value = null
            token.value = null
        }
    }

    const initializeAuth = () => {
        const storedToken = localStorage.getItem('token')
        const storedUser = localStorage.getItem('user')
        const storedOrg = localStorage.getItem('organization')

        if (storedToken && storedUser) {
            token.value = storedToken
            user.value = JSON.parse(storedUser)
            // Update HTTP service with stored token
            authService.setToken(storedToken)
        }

        if (storedOrg) {
            organization.value = JSON.parse(storedOrg)
        }
    }

    return {
        user,
        organization,
        token,
        isAuthenticated,
        login,
        register,
        createOrganization,
        logout,
        initializeAuth
    }
})
