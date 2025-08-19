import { computed, ref, onMounted } from 'vue'
import { useAuthStore } from '@/stores/auth'
import { authService } from '@/services/authService'
import { UserStatus, AccountType } from '@/types/global'

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
      const storedToken = localStorage.getItem('token')
      const storedUser = localStorage.getItem('user')

      if (storedToken && storedUser) {
        // Update HTTP service with stored token before validation
        authService.setToken(storedToken)

        // Validate token in background
        const result = await authService.validateToken()
        if (result.success && result.data) {
          // Token is valid, restore session
          authStore.setUser({
            id: result.data.userId,
            name: result.data.name,
            email: result.data.email,
            phone: result.data.phone,
            status: UserStatus.ACTIVE,
            accountType: AccountType.USER
          })
          authStore.setToken(result.data.token)
          authStore.setOrganization(null)

          // Update HTTP service with fresh token
          authService.setToken(result.data.token)

          // Update localStorage
          localStorage.setItem('token', result.data.token)
          localStorage.setItem('user', JSON.stringify(authStore.user))

          isInitialized.value = true
          return true
        } else {
          // Token invalid, clear session
          clearSession()
          isInitialized.value = true
          return false
        }
      } else {
        // No stored session
        isInitialized.value = true
        return false
      }
    } catch (error) {
      console.error('Auth initialization failed:', error)
      clearSession()
      isInitialized.value = true
      return false
    } finally {
      isLoading.value = false
    }
  }

  // Clear authentication session
  const clearSession = () => {
    authStore.clearSession()
    localStorage.removeItem('token')
    localStorage.removeItem('user')
    localStorage.removeItem('organization')
    // Clear HTTP service token
    authService.setToken('')
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
          status: UserStatus.ACTIVE,
          accountType: AccountType.USER
        }

        // Update store and localStorage
        authStore.setUser(userData)
        authStore.setToken(result.data.token)
        localStorage.setItem('token', result.data.token)
        localStorage.setItem('user', JSON.stringify(userData))

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
          status: UserStatus.ACTIVE,
          accountType: AccountType.USER
        }

        // Update store and localStorage
        authStore.setUser(userData)
        authStore.setToken(result.data.token)
        localStorage.setItem('token', result.data.token)
        localStorage.setItem('user', JSON.stringify(userData))

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
      console.error('Logout error:', error)
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
    if (!isInitialized.value) {
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
