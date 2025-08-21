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

// Backend FilterRequest structure for advanced filtering
export interface FilterRequest {
  // Basic filters
  category?: string
  brand?: string
  supplierName?: string
  color?: string
  size?: string
  searchTerm?: string // For name, SKU, or barcode search

  // Stock filters
  lowStock?: boolean
  outOfStock?: boolean
  minStock?: number
  maxStock?: number

  // Price filters
  minCostPrice?: number
  maxCostPrice?: number
  minSellingPrice?: number
  maxSellingPrice?: number

  // Date filters
  createdAfter?: string // LocalDate
  createdBefore?: string // LocalDate
  expiryAfter?: string // LocalDate
  expiryBefore?: string // LocalDate

  // Status filters
  isActive?: boolean
  isPerishable?: boolean
  hasDiscount?: boolean

  // Expiry status
  expiryStatus?: string // "expiring", "expired", "fresh"
  expiryDays?: number // Days for expiry calculation

  // Sorting
  sortBy?: string // Field to sort by
  sortDirection?: string // "asc" or "desc"
}

// Backend PagedResponse structure for paginated results
export interface PagedResponse<T> {
  content: T[]
  page: number
  size: number
  totalElements: number
  totalPages: number
  first: boolean
  last: boolean
  hasNext: boolean
  hasPrevious: boolean
  numberOfElements: number
  empty: boolean
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

  // Get inventory item by SKU within an inventory
  async getInventoryItemBySku(sku: string, inventoryId: string): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.get<InventoryItem>(`/inventory-items/sku/${sku}/inventory/${inventoryId}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get inventory item by barcode within an inventory
  async getInventoryItemByBarcode(barcode: string, inventoryId: string): Promise<ApiResponse<InventoryItem>> {
    try {
      const response = await this.axios.get<InventoryItem>(`/inventory-items/barcode/${barcode}/inventory/${inventoryId}`)
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

  // Get all inventory items for a specific inventory (basic)
  async getInventoryItems(inventoryId: string): Promise<ApiResponse<InventoryItem[]>> {
    try {
      const response = await this.axios.get<InventoryItem[]>(`/inventories/${inventoryId}/items`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get inventory items with comprehensive filtering and pagination
  async filterItemsPaginated(
    inventoryId: string,
    filterRequest: FilterRequest,
    page: number = 0,
    size: number = 20,
    sortBy: string = 'createdAt',
    sortDirection: string = 'desc'
  ): Promise<ApiResponse<PagedResponse<InventoryItem>>> {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        size: size.toString(),
        sortBy,
        sortDirection
      })
      // Add filter parameters
      if (filterRequest.category) params.append('category', filterRequest.category)
      if (filterRequest.brand) params.append('brand', filterRequest.brand)
      if (filterRequest.supplierName) params.append('supplier', filterRequest.supplierName)
      if (filterRequest.color) params.append('color', filterRequest.color)
      if (filterRequest.size) params.append('itemSize', filterRequest.size)
      if (filterRequest.searchTerm) params.append('search', filterRequest.searchTerm)
      if (filterRequest.isActive !== undefined) params.append('activeOnly', filterRequest.isActive.toString())
      if (filterRequest.isPerishable !== undefined) params.append('perishable', filterRequest.isPerishable.toString())
      if (filterRequest.lowStock !== undefined) params.append('lowStock', filterRequest.lowStock.toString())
      if (filterRequest.outOfStock !== undefined) params.append('outOfStock', filterRequest.outOfStock.toString())
      if (filterRequest.hasDiscount !== undefined) params.append('hasDiscount', filterRequest.hasDiscount.toString())
      if (filterRequest.minStock !== undefined) params.append('minStock', filterRequest.minStock.toString())
      if (filterRequest.maxStock !== undefined) params.append('maxStock', filterRequest.maxStock.toString())
      if (filterRequest.minCostPrice !== undefined) params.append('minCostPrice', filterRequest.minCostPrice.toString())
      if (filterRequest.maxCostPrice !== undefined) params.append('maxCostPrice', filterRequest.maxCostPrice.toString())
      if (filterRequest.minSellingPrice !== undefined) params.append('minSellingPrice', filterRequest.minSellingPrice.toString())
      if (filterRequest.maxSellingPrice !== undefined) params.append('maxSellingPrice', filterRequest.maxSellingPrice.toString())
      if (filterRequest.createdAfter) params.append('createdAfter', filterRequest.createdAfter)
      if (filterRequest.createdBefore) params.append('createdBefore', filterRequest.createdBefore)
      if (filterRequest.expiryAfter) params.append('expiryAfter', filterRequest.expiryAfter)
      if (filterRequest.expiryBefore) params.append('expiryBefore', filterRequest.expiryBefore)
      if (filterRequest.expiryStatus) params.append('expiryStatus', filterRequest.expiryStatus)
      if (filterRequest.expiryDays !== undefined) params.append('expiryDays', filterRequest.expiryDays.toString())

      const response = await this.axios.get<PagedResponse<InventoryItem>>(`/inventory-items/inventory/${inventoryId}?${params}`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get total item count in an inventory
  async getItemCountByInventory(inventoryId: string): Promise<ApiResponse<number>> {
    try {
      const response = await this.axios.get<number>(`/inventory-items/inventory/${inventoryId}/count`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get active item count in an inventory
  async getActiveItemCountByInventory(inventoryId: string): Promise<ApiResponse<number>> {
    try {
      const response = await this.axios.get<number>(`/inventory-items/inventory/${inventoryId}/count/active`)
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  // Get all available units
  async getAvailableUnits(): Promise<ApiResponse<Unit[]>> {
    try {
      const response = await this.axios.get<Unit[]>('/inventory-items/units')
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }
}

// Export singleton instance
export const inventoryItemService = new InventoryItemService()
