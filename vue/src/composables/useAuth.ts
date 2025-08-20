import { computed, ref, onMounted } from 'vue'
import { useAuthStore } from '@/stores/auth'
import { authService } from '@/services/authService'
import type { OrganizationResponse } from '@/services/organizationService'
import { toast } from 'vue-sonner'

export function useAuth() {
  const authStore = useAuthStore()
  const isLoading = ref(false)
  const isInitialized = ref(false)

  // Reactive authentication state
  const user = computed(() => authStore.user)
  const organization = computed(() => authStore.organization)
  const isAuthenticated = computed(() => authStore.isAuthenticated)
  const token = computed(() => authStore.token)

  // Initialize authentication state (non-blocking)
  const initialize = async () => {
    if (isInitialized.value) return true

    isLoading.value = true
    try {
      // Initialize token from localStorage via store
      authStore.initializeToken()
      const storedToken = authStore.token

      if (storedToken) {
        // Update HTTP service with stored token before validation
        authService.setToken(storedToken)

        // Validate token in background
        const result = await authService.validateToken()
        if (result.data) {
          // Token is valid, restore session
          authStore.setUser({
            id: result.data.userId,
            name: result.data.name,
            email: result.data.email,
            phone: result.data.phone,
            status: result.data.status,
            accountType: result.data.accountType
          })
          authStore.setToken(result.data.token)

          // Check if organization is already selected from localStorage
          const storedOrg = localStorage.getItem('selected_organization')
          if (storedOrg) {
            try {
              const org = JSON.parse(storedOrg) as OrganizationResponse
              authStore.setOrganization(org)
            } catch (err) {
              toast.error('Error parsing stored organization')
            }
          }

          // Update HTTP service with fresh token
          authService.setToken(result.data.token)

          isInitialized.value = true
          return true
        } else {
          isInitialized.value = true
          return false
        }
      } else {
        // No stored session
        isInitialized.value = true
        return false
      }
    } catch (error) {
      isInitialized.value = true
      return false
    } finally {
      isLoading.value = false
    }
  }

  // Clear authentication session
  const clearSession = () => {
    authStore.clearSession()
    // Token is already cleared in the store's clearSession method
  }

  // Login
  const login = async (emailOrPhone: string, password: string) => {
    isLoading.value = true
    try {
      const result = await authService.login({ emailOrPhone, password })

      if (result.success && result.data) {
        // Create user object from login response
        const userData = {
          id: result.data.userId,
          name: result.data.name,
          email: result.data.email,
          phone: result.data.phone,
          status: result.data.status,
          accountType: result.data.accountType
        }
        // Update store (token will be stored in localStorage via store)
        authStore.setUser(userData)
        authStore.setToken(result.data.token)

        // Update HTTP service token
        authService.setToken(result.data.token)

        isInitialized.value = true
      }
      return result
    } finally {
      isLoading.value = false
    }
  }

  // Register
  const register = async (name: string, email: string, phone: string, password: string) => {
    isLoading.value = true
    try {
      const result = await authService.register({ name, email, phone, password })
      if (result.success && result.data) {
        // Create user object from register response
        const userData = {
          id: result.data.userId,
          name: result.data.name,
          email: result.data.email,
          phone: result.data.phone,
          status: result.data.status,
          accountType: result.data.accountType
        }

        // Update store (token will be stored in localStorage via store)
        authStore.setUser(userData)
        authStore.setToken(result.data.token)

        // Update HTTP service token
        authService.setToken(result.data.token)

        isInitialized.value = true
      }
      return result
    } finally {
      isLoading.value = false
    }
  }

  // Logout
  const logout = async () => {
    isLoading.value = true
    try {
      await authService.logout()
    } catch (error) {
      toast.error('Logout error')
    } finally {
      clearSession()
      isInitialized.value = false
    }
  }

  // Check current authentication status
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

  // Auto-initialize when composable is used
  onMounted(() => {
    // Only auto-initialize if not already authenticated and not already initialized
    if (!isInitialized.value && !authStore.isAuthenticated) {
      initialize()
    }
  })

  return {
    // State
    user,
    organization,
    isAuthenticated,
    token,
    isLoading,
    isInitialized,

    // Methods
    initialize,
    login,
    register,
    logout,
    checkAuthStatus
  }
}
