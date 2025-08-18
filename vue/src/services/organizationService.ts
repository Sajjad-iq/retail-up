import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { ApiResponse } from './index'

// Backend CreateOrganizationRequest structure
export interface CreateOrganizationRequest {
    userId: string
    name: string
    domain: string
    description?: string
    address?: string
    phone: string
    email?: string
}

// Backend OrganizationResponse structure
export interface OrganizationResponse {
    id: string
    name: string
    domain: string
    description?: string
    address?: string
    phone: string
    status: OrganizationStatus
    createdAt: string // LocalDateTime from backend
    updatedAt: string // LocalDateTime from backend
    createdBy: string
}

// Backend OrganizationStatus enum
export enum OrganizationStatus {
    ACTIVE = 'ACTIVE',
    DISABLED = 'DISABLED',
    PENDING = 'PENDING',
    REJECTED = 'REJECTED',
    SUSPENDED = 'SUSPENDED',
    DELETED = 'DELETED'
}

// Backend UpdateOrganizationRequest structure
export interface UpdateOrganizationRequest {
    name?: string
    domain?: string
    description?: string
    address?: string
    phone?: string
    email?: string
}

class OrganizationService {
    private axios = httpService.getAxiosInstance()

    async createOrganization(orgData: CreateOrganizationRequest): Promise<ApiResponse<OrganizationResponse>> {
        try {
            const response = await this.axios.post<OrganizationResponse>('/organizations', orgData)
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async getOrganization(id: string): Promise<ApiResponse<OrganizationResponse>> {
        try {
            const response = await this.axios.get<OrganizationResponse>(`/organizations/${id}`)
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async updateOrganization(id: string, orgData: UpdateOrganizationRequest): Promise<ApiResponse<OrganizationResponse>> {
        try {
            const response = await this.axios.put<OrganizationResponse>(`/organizations/${id}`, orgData)
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async deleteOrganization(id: string): Promise<ApiResponse<void>> {
        try {
            await this.axios.delete(`/organizations/${id}`)
            return { success: true }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }

    async getOrganizations(): Promise<ApiResponse<OrganizationResponse[]>> {
        try {
            const response = await this.axios.get<OrganizationResponse[]>('/organizations')
            return { success: true, data: response.data }
        } catch (error: any) {
            const apiError = ErrorHandler.handleApiError(error)
            return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
        }
    }
}

// Export singleton instance
export const organizationService = new OrganizationService()
