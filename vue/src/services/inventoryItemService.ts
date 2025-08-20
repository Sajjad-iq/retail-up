import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { InventoryItem, Money, Unit } from '@/types/global'
import type { ApiResponse } from '.'

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
  userId: string // Required by backend validation
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

class InventoryItemService {
  private axios = httpService.getAxiosInstance()

  // Create a new inventory item
  async createInventoryItem(itemData: CreateInventoryItemRequest): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.post<InventoryItem>('/inventory-items', itemData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get a specific inventory item by ID
  async getInventoryItem(id: string): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.get<InventoryItem>(`/inventory-items/${id}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Update an existing inventory item
  async updateInventoryItem(id: string, itemData: UpdateInventoryItemRequest): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.put<InventoryItem>(`/inventory-items/${id}`, itemData)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Delete an inventory item
  async deleteInventoryItem(id: string): Promise<ApiResponse<void>> {
    try {
      await this.axios.delete(`/inventory-items/${id}`)
      return { success: true }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get all inventory items for a specific inventory
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
export const inventoryItemService = new InventoryItemService()
