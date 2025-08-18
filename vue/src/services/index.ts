// Shared Types
export interface ApiResponse<T> {
    success: boolean
    data?: T
    error?: string
    message?: string
}

// Currency Enum (from backend)
export enum Currency {
    USD = 'USD',
    EUR = 'EUR',
    GBP = 'GBP',
    CAD = 'CAD',
    AUD = 'AUD',
    JPY = 'JPY',
    CNY = 'CNY',
    INR = 'INR',
    CHF = 'CHF',
    AED = 'AED',
    SAR = 'SAR',
    ZAR = 'ZAR',
    BRL = 'BRL',
    MXN = 'MXN',
    TRY = 'TRY',
    NGN = 'NGN',
    IQD = 'IQD'
}

// Unit Enum (from backend)
export enum Unit {
    PIECES = 'PIECES',
    PAIRS = 'PAIRS',
    SETS = 'SETS',
    BOXES = 'BOXES',
    PACKS = 'PACKS',
    GRAMS = 'GRAMS',
    KILOGRAMS = 'KILOGRAMS',
    POUNDS = 'POUNDS',
    MILLILITERS = 'MILLILITERS',
    LITERS = 'LITERS',
    BOTTLES = 'BOTTLES',
    CANS = 'CANS',
    BAGS = 'BAGS'
}

// User Status Enum (from backend)
export enum UserStatus {
    ACTIVE = 'ACTIVE',
    INACTIVE = 'INACTIVE',
    LOCKED = 'LOCKED',
    PENDING = 'PENDING'
}

// Account Type Enum (from backend)
export enum AccountType {
    USER = 'USER',
    EMPLOYEE = 'EMPLOYEE'
}

// Money Type (from backend)
export interface Money {
    amount: number
    currency: Currency
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
    UpdateOrganizationRequest,
    OrganizationStatus
} from './organizationService'

// Inventory Service
export { inventoryService } from './inventoryService'
export type {
    CreateInventoryRequest,
    Inventory,
    InventoryItem,
    UpdateInventoryRequest,
    CreateInventoryItemRequest,
    UpdateInventoryItemRequest
} from './inventoryService'
