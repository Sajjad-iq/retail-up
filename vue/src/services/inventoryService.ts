import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { ApiResponse } from './index'

// Backend CreateInventoryRequest structure
export interface CreateInventoryRequest {
  userId: string
  organizationId: string
  name: string
  description?: string
  location?: string
}

// Backend Inventory entity structure
export interface Inventory {
  id: string
  name: string
  description?: string
  location?: string
  isActive: boolean
  organizationId: string
  createdAt: string // LocalDateTime from backend
  updatedAt: string // LocalDateTime from backend
  organization?: any // Organization entity
  createdBy?: any // User entity
  inventoryItems?: InventoryItem[]
}

// Backend InventoryItem structure (basic)
export interface InventoryItem {
  id: string
  name: string
  description?: string
  quantity: number
  unitPrice: number
  category?: string
  sku?: string
  barcode?: string
  isActive: boolean
  inventoryId: string
  createdAt: string
  updatedAt: string
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

  async createInventory(inventoryData: CreateInventoryRequest): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.post<Inventory>('/inventories', inventoryData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async getInventory(id: string): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.get<Inventory>(`/inventories/${id}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async updateInventory(id: string, inventoryData: UpdateInventoryRequest): Promise<ApiResponse<Inventory>> {
    try {
      const response = await this.axios.put<Inventory>(`/inventories/${id}`, inventoryData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async deleteInventory(id: string): Promise<ApiResponse<void>> {
    try {
      await this.axios.delete(`/inventories/${id}`)
      return { success: true }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

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

  async getInventoryItems(inventoryId: string): Promise<ApiResponse<InventoryItem[]>> {
    try {
      const response = await this.axios.get<InventoryItem[]>(`/inventories/${inventoryId}/items`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }
}

// Export singleton instance
export const inventoryService = new InventoryService()
