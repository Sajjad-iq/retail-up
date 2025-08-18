
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


// Backend OrganizationStatus enum
export enum OrganizationStatus {
    ACTIVE = 'ACTIVE',
    DISABLED = 'DISABLED',
    PENDING = 'PENDING',
    REJECTED = 'REJECTED',
    SUSPENDED = 'SUSPENDED',
    DELETED = 'DELETED'
}
