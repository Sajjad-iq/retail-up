import { useQuery, useMutation } from '@tanstack/vue-query'
import { useAuthStore } from '@/stores/auth'
import { inventoryItemService } from '@/services/inventoryItemService'
import { toast } from 'vue-sonner'
import { queryClient, queryKeys, queryUtils } from '@/config/query'
import type {
    CreateInventoryItemRequest,
    UpdateInventoryItemRequest,
    FilterRequest,
} from '@/services/inventoryItemService'

export function useInventoryItems() {
    const authStore = useAuthStore()

    // Get inventory items with pagination and filtering
    const useInventoryItemsList = (
        inventoryId: string,
        filters: FilterRequest = {},
        page: number = 0,
        size: number = 20,
        sortBy: string = 'createdAt',
        sortDirection: string = 'desc',
        enabled: boolean = true
    ) => {
        console.log("ID FROM COMPOSABLE ", inventoryId);
        return useQuery({
            queryKey: queryKeys.inventoryItems.list(inventoryId, filters, page, size, sortBy, sortDirection),
            queryFn: () => inventoryItemService.filterItemsPaginated(inventoryId, filters, page, size, sortBy, sortDirection),
            enabled: enabled && !!inventoryId,
        })
    }

    // Get a single inventory item by ID
    const useInventoryItem = (id: string, enabled: boolean = true) => {
        return useQuery({
            queryKey: queryKeys.inventoryItems.details(id),
            queryFn: () => inventoryItemService.getInventoryItem(id),
            enabled: enabled && !!id,
        })
    }

    // Get inventory item by SKU
    const useInventoryItemBySku = (sku: string, inventoryId: string, enabled: boolean = true) => {
        return useQuery({
            queryKey: queryKeys.inventoryItems.bySku(sku, inventoryId),
            queryFn: () => inventoryItemService.getInventoryItemBySku(sku, inventoryId),
            enabled: enabled && !!sku && !!inventoryId,
        })
    }

    // Get inventory item by barcode
    const useInventoryItemByBarcode = (barcode: string, inventoryId: string, enabled: boolean = true) => {
        return useQuery({
            queryKey: queryKeys.inventoryItems.byBarcode(barcode, inventoryId),
            queryFn: () => inventoryItemService.getInventoryItemByBarcode(barcode, inventoryId),
            enabled: enabled && !!barcode && !!inventoryId,
        })
    }

    // Get item counts
    const useItemCounts = (inventoryId: string, enabled: boolean = true) => {
        const totalCountQuery = useQuery({
            queryKey: [...queryKeys.inventoryItems.counts(inventoryId), 'total'],
            queryFn: () => inventoryItemService.getItemCountByInventory(inventoryId),
            enabled: enabled && !!inventoryId,
        })

        const activeCountQuery = useQuery({
            queryKey: [...queryKeys.inventoryItems.counts(inventoryId), 'active'],
            queryFn: () => inventoryItemService.getActiveItemCountByInventory(inventoryId),
            enabled: enabled && !!inventoryId,
        })

        return {
            totalCount: totalCountQuery,
            activeCount: activeCountQuery,
            isLoading: totalCountQuery.isLoading.value || activeCountQuery.isLoading.value,
            error: totalCountQuery.error.value || activeCountQuery.error.value
        }
    }

    // Get available units
    const useAvailableUnits = () => {
        return useQuery({
            queryKey: queryKeys.inventoryItems.units(),
            queryFn: () => inventoryItemService.getAvailableUnits(),
        })
    }

    // Create inventory item mutation
    const useCreateInventoryItem = () => {
        return useMutation({
            mutationFn: (itemData: Omit<CreateInventoryItemRequest, 'userId'>) => {
                if (!authStore.user?.id) {
                    throw new Error('User not authenticated')
                }
                return inventoryItemService.createInventoryItem({
                    ...itemData,
                    userId: authStore.user.id
                })
            },
            onSuccess: (result, variables) => {
                if (result.success && result.data) {
                    toast.success(`Item "${result.data.name}" created successfully`)

                    // Invalidate and refetch relevant queries using global utils
                    queryUtils.invalidateInventoryItems(variables.inventoryId)
                } else {
                    toast.error(result.error || 'Failed to create item')
                }
            },
            onError: (error) => {
                toast.error('An error occurred while creating the item')
            }
        })
    }

    // Update inventory item mutation
    const useUpdateInventoryItem = () => {
        return useMutation({
            mutationFn: ({ id, itemData, inventoryId }: {
                id: string;
                itemData: Omit<UpdateInventoryItemRequest, 'userId'>;
                inventoryId: string;
            }) => {
                if (!authStore.user?.id) {
                    throw new Error('User not authenticated')
                }
                return inventoryItemService.updateInventoryItem(id, {
                    ...itemData,
                    userId: authStore.user.id
                })
            },
            onSuccess: (result, variables) => {
                if (result.success && result.data) {
                    toast.success(`Item "${result.data.name}" updated successfully`)

                    // Invalidate and refetch relevant queries using global utils
                    queryClient.invalidateQueries({ queryKey: queryKeys.inventoryItems.details(variables.id) })
                    queryUtils.invalidateInventoryItems(variables.inventoryId)
                } else {
                    toast.error(result.error || 'Failed to update item')
                }
            },
            onError: (error) => {
                toast.error('An error occurred while updating the item')
            }
        })
    }

    // Delete inventory item mutation
    const useDeleteInventoryItem = () => {
        return useMutation({
            mutationFn: ({ id, inventoryId }: { id: string; inventoryId: string }) =>
                inventoryItemService.deleteInventoryItem(id),
            onSuccess: (result, variables) => {
                if (result.success) {
                    toast.success('Item deleted successfully')

                    // Invalidate and refetch relevant queries using global utils
                    queryUtils.invalidateInventoryItems(variables.inventoryId)

                    // Remove from cache
                    queryClient.removeQueries({ queryKey: queryKeys.inventoryItems.details(variables.id) })
                } else {
                    toast.error(result.error || 'Failed to delete item')
                }
            },
            onError: (error) => {
                toast.error('An error occurred while deleting the item')
                console.error('Delete inventory item error:', error)
            }
        })
    }

    return {
        // Query hooks
        useInventoryItemsList,
        useInventoryItem,
        useInventoryItemBySku,
        useInventoryItemByBarcode,
        useItemCounts,
        useAvailableUnits,

        // Mutation hooks
        useCreateInventoryItem,
        useUpdateInventoryItem,
        useDeleteInventoryItem,

        // Utility functions (re-export global utils)
        invalidateInventoryItemQueries: queryUtils.invalidateInventoryItems,
        prefetchInventoryItem: queryUtils.prefetchInventoryItem,

        // Query keys for external use
        queryKeys: queryKeys.inventoryItems
    }
}
