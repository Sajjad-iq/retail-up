import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import { authService } from '@/services/authService'
import type { UserStatus, AccountType } from '@/services'
import type { User, Organization } from '@/types/global'

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
                // Note: Backend LoginResponse doesn't include all user fields
                // We'll need to fetch the full user profile separately
                user.value = {
                    id: result.data.userId,
                    name: result.data.name,
                    email: result.data.email,
                    phone: result.data.phone,
                    status: 'ACTIVE' as UserStatus, // Default status
                    accountType: 'USER' as AccountType, // Default account type
                    // createdAt and updatedAt are nullable in backend, so we'll set them when we fetch full user profile
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
                    phone: result.data.phone,
                    status: 'ACTIVE' as UserStatus, // Default status
                    accountType: 'USER' as AccountType, // Default account type
                    // createdAt and updatedAt are nullable in backend, so we'll set them when we fetch full user profile
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
        logout,
        initializeAuth
    }
})
