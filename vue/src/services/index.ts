// Shared Types
export interface ApiResponse<T> {
    success: boolean
    data?: T
    error?: string
    message?: string
}

// Export all services for easier imports
export { inventoryService } from './inventoryService'
export { inventoryItemService } from './inventoryItemService'
export { organizationService } from './organizationService'
export { authService } from './authService'
