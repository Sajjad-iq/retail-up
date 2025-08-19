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

    console.log('useAuth - initialize called')
    isLoading.value = true
    try {
      // Initialize token from localStorage via store
      console.log('useAuth - calling authStore.initializeToken()')
      authStore.initializeToken()
      const storedToken = authStore.token
      console.log('useAuth - stored token from store:', storedToken)

      if (storedToken) {
        console.log('useAuth - token found, updating HTTP service')
        console.log('useAuth - token being sent for validation:', storedToken)
        console.log('useAuth - token length:', storedToken.length)

        // Update HTTP service with stored token before validation
        authService.setToken(storedToken)

        // Add a small delay to ensure the token is properly set
        await new Promise(resolve => setTimeout(resolve, 100))

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
      console.log('useAuth - login called with:', emailOrPhone)
      const result = await authService.login({ emailOrPhone, password })
      console.log('useAuth - login result:', result)

      if (result.success && result.data) {
        console.log('useAuth - login successful, token received:', result.data.token)

        // Create user object from login response
        const userData = {
          id: result.data.userId,
          name: result.data.name,
          email: result.data.email,
          phone: result.data.phone,
          status: UserStatus.ACTIVE,
          accountType: AccountType.USER
        }

        console.log('useAuth - setting user data:', userData)
        console.log('useAuth - setting token:', result.data.token)

        // Update store (token will be stored in localStorage via store)
        authStore.setUser(userData)
        authStore.setToken(result.data.token)

        // Update HTTP service token
        authService.setToken(result.data.token)

        isInitialized.value = true
        console.log('useAuth - login completed, isInitialized:', isInitialized.value)
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
    // Only auto-initialize if not already authenticated and not already initialized
    if (!isInitialized.value && !authStore.isAuthenticated) {
      console.log('useAuth - onMounted: auto-initializing')
      initialize()
    } else {
      console.log('useAuth - onMounted: skipping auto-initialization (already authenticated or initialized)')
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
