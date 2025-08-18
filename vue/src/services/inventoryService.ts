import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { ApiResponse } from './index'
import type { Money, Unit, Currency } from './index'

// Backend CreateInventoryRequest structure
export interface CreateInventoryRequest {
  userId: string
  organizationId: string
  name: string
  description?: string
  location?: string
}

// Backend Inventory entity structure (complete match)
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
  createdBy: any // User entity - was missing!
  inventoryItems?: InventoryItem[]
}

// Backend InventoryItem structure (complete match)
export interface InventoryItem {
  id: string
  name: string
  description?: string
  sku?: string
  productCode?: string
  barcode?: string
  category?: string
  brand?: string
  unit: Unit
  weight?: number
  dimensions?: string
  color?: string
  size?: string
  currentStock: number
  minimumStock?: number
  maximumStock?: number
  costPrice?: Money
  sellingPrice: Money
  discountPrice?: number
  discountStartDate?: string // LocalDateTime
  discountEndDate?: string // LocalDateTime
  supplierName?: string
  isPerishable: boolean
  expiryDate?: string // LocalDate
  isActive: boolean
  totalSold: number
  totalRevenue: number
  lastSoldDate?: string // LocalDateTime
  inventoryId: string
  createdAt: string // LocalDateTime
  updatedAt: string // LocalDateTime
  inventory?: Inventory
  createdBy?: any // User entity
}

// Backend CreateInventoryItemRequest structure (complete match)
export interface CreateInventoryItemRequest {
  userId: string
  id?: string // temporary id for excel upload
  inventoryId: string
  name: string
  description?: string
  sku?: string
  productCode?: string
  barcode?: string
  category?: string
  brand?: string
  unit: Unit
  weight?: number
  dimensions?: string
  color?: string
  size?: string
  currentStock: number
  minimumStock?: number
  maximumStock?: number
  costPrice?: Money
  sellingPrice: Money
  discountPrice?: number
  discountStartDate?: string // LocalDateTime
  discountEndDate?: string // LocalDateTime
  supplierName?: string
  isPerishable?: boolean
  expiryDate?: string // LocalDate
}

// Backend UpdateInventoryItemRequest structure
export interface UpdateInventoryItemRequest {
  name?: string
  description?: string
  sku?: string
  productCode?: string
  barcode?: string
  category?: string
  brand?: string
  unit?: Unit
  weight?: number
  dimensions?: string
  color?: string
  size?: string
  currentStock?: number
  minimumStock?: number
  maximumStock?: number
  costPrice?: Money
  sellingPrice?: Money
  discountPrice?: number
  discountStartDate?: string
  discountEndDate?: string
  supplierName?: string
  isPerishable?: boolean
  expiryDate?: string
  isActive?: boolean
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

  // Inventory Item specific methods
  async createInventoryItem(itemData: CreateInventoryItemRequest): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.post<InventoryItem>('/inventory-items', itemData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async getInventoryItem(id: string): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.get<InventoryItem>(`/inventory-items/${id}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async updateInventoryItem(id: string, itemData: UpdateInventoryItemRequest): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.put<InventoryItem>(`/inventory-items/${id}`, itemData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  async deleteInventoryItem(id: string): Promise<ApiResponse<void>> {
    try {
      await this.axios.delete(`/inventory-items/${id}`)
      return { success: true }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }
}

// Export singleton instance
export const inventoryService = new InventoryService()
