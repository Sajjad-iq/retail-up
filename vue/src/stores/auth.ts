import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import { authService } from '@/services/authService'
import type { User, Organization, UserStatus, AccountType } from '@/types/global'

export const useAuthStore = defineStore('auth', () => {
    const user = ref<User | null>(null)
    const organization = ref<Organization | null>(null)
    const token = ref<string | null>(null)
    const isAuthenticated = computed(() => !!token.value && !!user.value)

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
                    status: '' as UserStatus, // Default status
                    accountType: '' as AccountType, // Default account type
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
                    status: '' as UserStatus, // Default status
                    accountType: '' as AccountType, // Default account type
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
            localStorage.removeItem('token')
            localStorage.removeItem('user')
            localStorage.removeItem('organization')
        }
    }

    const initializeAuth = async () => {
        const storedToken = localStorage.getItem('token')
        const storedUser = localStorage.getItem('user')
        const storedOrg = localStorage.getItem('organization')

        if (storedToken && storedUser) {
            try {
                // Validate the stored token with the backend
                const result = await authService.validateToken()
                if (result.success && result.data) {
                    // Token is valid, restore the session with fresh user data
                    token.value = result.data.token
                    user.value = {
                        id: result.data.userId,
                        name: result.data.name,
                        email: result.data.email,
                        phone: result.data.phone,
                        status: 'ACTIVE' as UserStatus, // Default status
                        accountType: 'USER' as AccountType, // Default account type
                    }
                    // Update HTTP service with stored token
                    authService.setToken(result.data.token)

                    // Update localStorage with fresh user data
                    localStorage.setItem('token', result.data.token)
                    localStorage.setItem('user', JSON.stringify(user.value))
                } else {
                    // Token is invalid, clear stored data
                    localStorage.removeItem('token')
                    localStorage.removeItem('user')
                    localStorage.removeItem('organization')
                    token.value = null
                    user.value = null
                    organization.value = null
                }
            } catch (error) {
                // Token validation failed, clear stored data
                localStorage.removeItem('token')
                localStorage.removeItem('user')
                localStorage.removeItem('organization')
                token.value = null
                user.value = null
                organization.value = null
            }
        }

        if (storedOrg) {
            organization.value = JSON.parse(storedOrg)
        }
    }

    // Method to check current authentication status
    const checkAuthStatus = async (): Promise<boolean> => {
        if (!token.value) return false

        try {
            const result = await authService.validateToken()
            if (!result.success) {
                // Token is invalid, logout
                await logout()
                return false
            }
            return true
        } catch (error) {
            // Token validation failed, logout
            await logout()
            return false
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
        initializeAuth,
        checkAuthStatus
    }
})
