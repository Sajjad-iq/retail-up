import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { ApiResponse } from './index'

// Backend LoginRequest uses emailOrPhone instead of email
export interface LoginRequest {
    emailOrPhone: string
    password: string
}

// Backend LoginResponse structure
export interface LoginResponse {
    token: string
    userId: string
    name: string
    email: string
    phone: string
    message: string
}

// Backend RegisterRequest structure
export interface RegisterRequest {
    name: string
    email: string
    phone: string
    password: string
}

// Backend ChangePasswordRequest structure
export interface ChangePasswordRequest {
    userId: string
    oldPassword: string
    newPassword: string
}

// Backend ChangePasswordResponse structure
export interface ChangePasswordResponse {
    success: boolean
    message: string
}

// Backend AuthResponse structure
export interface AuthResponse {
    success: boolean
    message: string
}

class AuthService {
    private axios = httpService.getAxiosInstance()

    async login(credentials: LoginRequest): Promise<ApiResponse<LoginResponse>> {
        try {
            const response = await this.axios.post<LoginResponse>('/auth/login', credentials)

            if (response.data.token) {
                // Update HTTP service with new token
                httpService.setAuthToken(response.data.token)
            }

            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async register(userData: RegisterRequest): Promise<ApiResponse<LoginResponse>> {
        try {
            const response = await this.axios.post<LoginResponse>('/auth/register', userData)

            if (response.data.token) {
                // Update HTTP service with new token
                httpService.setAuthToken(response.data.token)
            }

            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async changePassword(passwordData: ChangePasswordRequest): Promise<ApiResponse<ChangePasswordResponse>> {
        try {
            const response = await this.axios.post<ChangePasswordResponse>('/auth/change-password', passwordData)
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async userExists(emailOrPhone: string): Promise<ApiResponse<boolean>> {
        try {
            const response = await this.axios.get<boolean>('/auth/exists', {
                params: { emailOrPhone }
            })
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async validateToken(): Promise<ApiResponse<LoginResponse>> {
        try {
            const response = await this.axios.get<LoginResponse>('/auth/validate-token')
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async getCurrentUser(): Promise<ApiResponse<any>> {
        try {
            const response = await this.axios.get('/auth/me')
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async logout(): Promise<void> {
        try {
            await this.axios.post('/auth/logout')
        } catch (error) {
            console.error('Logout error:', error)
        } finally {
            httpService.clearAuthToken()
        }
    }

    // Method to check if user is authenticated
    isAuthenticated(): boolean {
        return !!localStorage.getItem('token')
    }

    // Method to get stored token
    getToken(): string | null {
        return localStorage.getItem('token')
    }

    // Method to set token manually (useful for initialization)
    setToken(token: string): void {
        httpService.setAuthToken(token)
    }
}

// Export singleton instance
export const authService = new AuthService()
