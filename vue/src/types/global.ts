import type { AccountType, UserStatus } from "@/services"
import type { OrganizationStatus } from "@/services/organizationService"

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