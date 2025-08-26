
export interface User {
    id: string
    name: string
    phone: string
    email: string
    status: UserStatus
    accountType: AccountType
    createdAt?: string // nullable in backend
    updatedAt?: string // nullable in backend
    lastLoginAt?: string
}

export interface Organization {
    id: string
    name: string
    domain: string
    description?: string
    address?: string
    phone: string
    status: OrganizationStatus
    createdAt: string
    updatedAt: string
    createdBy: string // User ID as string (from DTO), not User object
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
}

// Backend InventoryItem structure (complete match)
export interface InventoryItem {
    id: string
    name: string
    description?: string
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
    inventoryId: string
    createdAt: string // LocalDateTime
    updatedAt: string // LocalDateTime
    createdBy?: any // User entity
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

// Unit display names mapping (from backend)
export const UnitDisplayNames: Record<Unit, string> = {
    [Unit.PIECES]: 'pieces',
    [Unit.PAIRS]: 'pairs',
    [Unit.SETS]: 'sets',
    [Unit.BOXES]: 'boxes',
    [Unit.PACKS]: 'packs',
    [Unit.GRAMS]: 'grams',
    [Unit.KILOGRAMS]: 'kg',
    [Unit.POUNDS]: 'lbs',
    [Unit.MILLILITERS]: 'ml',
    [Unit.LITERS]: 'liters',
    [Unit.BOTTLES]: 'bottles',
    [Unit.CANS]: 'cans',
    [Unit.BAGS]: 'bags'
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


// Backend OrganizationStatus enum
export enum OrganizationStatus {
    ACTIVE = 'ACTIVE',
    DISABLED = 'DISABLED'
}
