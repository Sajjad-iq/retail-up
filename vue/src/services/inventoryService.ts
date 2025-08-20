import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { Inventory } from '@/types/global'
import type { ApiResponse } from '.'

// Backend CreateInventoryRequest structure
export interface CreateInventoryRequest {
  userId: string
  organizationId: string
  name: string
  description?: string
  location?: string
}

// Backend UpdateInventoryRequest structure
export interface UpdateInventoryRequest {
  name?: string
  description?: string
  location?: string
  isActive?: boolean
}

class InventoryService {
  private axios = httpService.getAxiosInstance()

  // Create a new inventory
  async createInventory(inventoryData: CreateInventoryRequest): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.post<Inventory>('/inventories', inventoryData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get a specific inventory by ID
  async getInventory(id: string): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.get<Inventory>(`/inventories/${id}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Update an existing inventory
  async updateInventory(id: string, inventoryData: UpdateInventoryRequest): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.put<Inventory>(`/inventories/${id}`, inventoryData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Delete an inventory (soft delete by setting isActive to false)
  async deleteInventory(id: string): Promise<ApiResponse<void>> {
    try {
      await this.axios.delete(`/inventories/${id}`)
      return { success: true }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get all inventories (optionally filtered by organization)
  async getInventories(organizationId?: string): Promise<ApiResponse<Inventory[]>> {
    try {
      const params = organizationId ? { organizationId } : {}
      const response = await this.axios.get<Inventory[]>('/inventories', { params })
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get all inventories for a specific organization
  async getInventoriesByOrganization(organizationId: string): Promise<ApiResponse<Inventory[]>> {
    try {
      const response = await this.axios.get<Inventory[]>(`/inventories/organization/${organizationId}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get active inventories for an organization
  async getActiveInventories(organizationId: string): Promise<ApiResponse<Inventory[]>> {
    try {
      const response = await this.axios.get<Inventory[]>(`/inventories/organization/${organizationId}/active`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Search inventories by name within an organization
  async searchInventories(organizationId: string, query: string): Promise<ApiResponse<Inventory[]>> {
    try {
      const response = await this.axios.get<Inventory[]>(`/inventories/organization/${organizationId}/search`, {
        params: { q: query }
      })
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Check if inventory exists by name within an organization
  async inventoryExistsByName(name: string, organizationId: string): Promise<ApiResponse<boolean>> {
    try {
      const response = await this.axios.get<boolean>(`/inventories/exists/name/${name}/organization/${organizationId}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get inventory count for an organization
  async getInventoryCount(organizationId: string): Promise<ApiResponse<number>> {
    try {
      const response = await this.axios.get<number>(`/inventories/organization/${organizationId}/count`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }
}

// Export singleton instance
export const inventoryService = new InventoryService()
