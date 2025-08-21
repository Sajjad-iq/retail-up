import { QueryClient } from '@tanstack/vue-query'

// Global QueryClient instance configuration
export const queryClient = new QueryClient({
    defaultOptions: {
        queries: {
            // Data is considered fresh for 5 minutes
            staleTime: 5 * 60 * 1000, // 5 minutes

            // Data is kept in cache for 10 minutes after becoming stale
            gcTime: 10 * 60 * 1000, // 10 minutes (formerly cacheTime)

            // Retry failed requests up to 2 times
            retry: 2,

            // Don't refetch when window regains focus
            refetchOnWindowFocus: false,

            // Don't refetch when reconnecting
            refetchOnReconnect: false,

            // Don't refetch on mount if data is fresh
            refetchOnMount: false,
        },
        mutations: {
            // Retry failed mutations only once
            retry: 3
        },
    },
})

// Query key factories for consistent caching
export const queryKeys = {
    // Inventory Items
    inventoryItems: {
        all: ['inventoryItems'] as const,
        lists: (inventoryId: string) => [...queryKeys.inventoryItems.all, 'list', inventoryId] as const,
        list: (inventoryId: string, filters: any, page: number, size: number, sortBy: string, sortDirection: string) =>
            [...queryKeys.inventoryItems.lists(inventoryId), { filters, page, size, sortBy, sortDirection }] as const,
        details: (id: string) => [...queryKeys.inventoryItems.all, 'detail', id] as const,
        bySku: (sku: string, inventoryId: string) => [...queryKeys.inventoryItems.all, 'sku', sku, inventoryId] as const,
        byBarcode: (barcode: string, inventoryId: string) => [...queryKeys.inventoryItems.all, 'barcode', barcode, inventoryId] as const,
        counts: (inventoryId: string) => [...queryKeys.inventoryItems.all, 'counts', inventoryId] as const,
        units: () => [...queryKeys.inventoryItems.all, 'units'] as const,
    },

    // Inventories
    inventories: {
        all: ['inventories'] as const,
        lists: () => [...queryKeys.inventories.all, 'list'] as const,
        details: (id: string) => [...queryKeys.inventories.all, 'detail', id] as const,
        byOrganization: (organizationId: string) => [...queryKeys.inventories.all, 'organization', organizationId] as const,
        active: (organizationId: string) => [...queryKeys.inventories.all, 'active', organizationId] as const,
    },

    // Users
    users: {
        all: ['users'] as const,
        details: (id: string) => [...queryKeys.users.all, 'detail', id] as const,
        current: () => [...queryKeys.users.all, 'current'] as const,
    },

    // Organizations
    organizations: {
        all: ['organizations'] as const,
        details: (id: string) => [...queryKeys.organizations.all, 'detail', id] as const,
        current: () => [...queryKeys.organizations.all, 'current'] as const,
    },

    // Settings
    settings: {
        all: ['settings'] as const,
        system: () => [...queryKeys.settings.all, 'system'] as const,
        business: () => [...queryKeys.settings.all, 'business'] as const,
    },
}

// Utility functions for query management
export const queryUtils = {
    // Invalidate all inventory item queries for a specific inventory
    invalidateInventoryItems: (inventoryId: string) => {
        queryClient.invalidateQueries({ queryKey: queryKeys.inventoryItems.lists(inventoryId) })
        queryClient.invalidateQueries({ queryKey: queryKeys.inventoryItems.counts(inventoryId) })
    },

    // Invalidate all inventory queries for an organization
    invalidateInventories: (organizationId: string) => {
        queryClient.invalidateQueries({ queryKey: queryKeys.inventories.byOrganization(organizationId) })
        queryClient.invalidateQueries({ queryKey: queryKeys.inventories.active(organizationId) })
    },

    // Prefetch an inventory item for better UX
    prefetchInventoryItem: (id: string) => {
        queryClient.prefetchQuery({
            queryKey: queryKeys.inventoryItems.details(id),
            queryFn: () => Promise.resolve(), // Placeholder - will be overridden by actual query
            staleTime: 5 * 60 * 1000,
        })
    },

    // Clear all queries (useful for logout)
    clearAll: () => {
        queryClient.clear()
    },

    // Reset all queries to initial state
    resetAll: () => {
        queryClient.resetQueries()
    },
}

// Export the queryClient instance for use in main.ts
export default queryClient
