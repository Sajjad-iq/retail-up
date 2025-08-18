// Shared Types
export interface ApiResponse<T> {
    success: boolean
    data?: T
    error?: string
    message?: string
}

// Authentication Service
export { authService } from './authService'
export type {
    LoginRequest,
    LoginResponse,
    RegisterRequest,
    ChangePasswordRequest,
    ChangePasswordResponse,
    AuthResponse
} from './authService'

// Organization Service
export { organizationService } from './organizationService'
export type {
    CreateOrganizationRequest,
    OrganizationResponse,
    UpdateOrganizationRequest
} from './organizationService'

// Inventory Service
export { inventoryService } from './inventoryService'
export type {
    CreateInventoryRequest,
    UpdateInventoryRequest,
    CreateInventoryItemRequest,
    UpdateInventoryItemRequest
} from './inventoryService'
