import { computed, ref, onMounted } from 'vue'
import { useAuthStore } from '@/stores/auth'
import { inventoryService } from '@/services/inventoryService'
import { toast } from 'vue-sonner'
import type { Inventory } from '@/types/global'
import type { CreateInventoryRequest, UpdateInventoryRequest } from '@/services/inventoryService'

export function useInventory() {
    const authStore = useAuthStore()
    const isLoading = ref(false)
    const inventories = ref<Inventory[]>([])
    const selectedInventory = ref<Inventory | null>(null)
    const error = ref<string | null>(null)

    // Computed properties
    const hasInventories = computed(() => inventories.value.length > 0)
    const isInventorySelected = computed(() => !!selectedInventory.value)
    const activeInventories = computed(() => inventories.value.filter(inv => inv.isActive))

    // Get all inventories for the current organization
    const fetchOrganizationInventories = async () => {
        if (!authStore.organization?.id) {
            const errorMsg = 'No organization selected'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.getInventoriesByOrganization(authStore.organization.id)

            if (result.success && result.data) {
                inventories.value = result.data
                return true
            } else {
                const errorMsg = result.error || 'Failed to fetch inventories'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while fetching inventories'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Get active inventories for the current organization
    const fetchActiveInventories = async () => {
        if (!authStore.organization?.id) {
            const errorMsg = 'No organization selected'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.getActiveInventories(authStore.organization.id)

            if (result.success && result.data) {
                inventories.value = result.data
                return true
            } else {
                const errorMsg = result.error || 'Failed to fetch active inventories'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while fetching active inventories'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Select an inventory
    const selectInventory = (inventory: Inventory) => {
        selectedInventory.value = inventory
    }

    // Get inventory by ID
    const getInventoryById = async (id: string): Promise<Inventory | null> => {
        try {
            const result = await inventoryService.getInventory(id)
            if (result.success && result.data) {
                return result.data
            } else {
                toast.error(result.error || 'Failed to fetch inventory')
                return null
            }
        } catch (err) {
            toast.error('An error occurred while fetching inventory')
            return null
        }
    }

    // Create a new inventory
    const createInventory = async (inventoryData: Omit<CreateInventoryRequest, 'userId' | 'organizationId'>) => {
        if (!authStore.user?.id || !authStore.organization?.id) {
            const errorMsg = 'User not authenticated or no organization selected'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.createInventory({
                userId: authStore.user.id,
                organizationId: authStore.organization.id,
                ...inventoryData
            })

            if (result.success && result.data) {
                // Add the new inventory to the list
                inventories.value.push(result.data)

                // Optionally select the new inventory
                selectInventory(result.data)

                toast.success(`Inventory "${result.data.name}" created successfully`)
                return true
            } else {
                const errorMsg = result.error || 'Failed to create inventory'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while creating inventory'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Update an existing inventory
    const updateInventory = async (id: string, inventoryData: UpdateInventoryRequest) => {
        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.updateInventory(id, inventoryData)

            if (result.success && result.data) {
                // Update the inventory in the list
                const index = inventories.value.findIndex(inv => inv.id === id)
                if (index !== -1) {
                    inventories.value[index] = result.data
                }

                // Update selected inventory if it's the one being updated
                if (selectedInventory.value?.id === id) {
                    selectInventory(result.data)
                }

                toast.success(`Inventory "${result.data.name}" updated successfully`)
                return true
            } else {
                const errorMsg = result.error || 'Failed to update inventory'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while updating inventory'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Delete an inventory (soft delete)
    const deleteInventory = async (id: string) => {
        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.deleteInventory(id)

            if (result.success) {
                // Remove the inventory from the list
                inventories.value = inventories.value.filter(inv => inv.id !== id)

                // Clear selection if the deleted inventory was selected
                if (selectedInventory.value?.id === id) {
                    selectedInventory.value = null
                }

                toast.success('Inventory deleted successfully')
                return true
            } else {
                const errorMsg = result.error || 'Failed to delete inventory'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while deleting inventory'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Search inventories by name
    const searchInventories = async (query: string) => {
        if (!authStore.organization?.id) {
            const errorMsg = 'No organization selected'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        }

        if (!query.trim()) {
            // If query is empty, fetch all inventories
            return await fetchOrganizationInventories()
        }

        isLoading.value = true
        error.value = null

        try {
            const result = await inventoryService.searchInventories(authStore.organization.id, query)

            if (result.success && result.data) {
                inventories.value = result.data
                return true
            } else {
                const errorMsg = result.error || 'Failed to search inventories'
                error.value = errorMsg
                toast.error(errorMsg)
                return false
            }
        } catch (err) {
            const errorMsg = 'An error occurred while searching inventories'
            error.value = errorMsg
            toast.error(errorMsg)
            return false
        } finally {
            isLoading.value = false
        }
    }

    // Check if inventory exists by name
    const checkInventoryExists = async (name: string): Promise<boolean> => {
        if (!authStore.organization?.id) {
            return false
        }

        try {
            const result = await inventoryService.inventoryExistsByName(name, authStore.organization.id)
            return result.success && result.data === true
        } catch (err) {
            return false
        }
    }

    // Get inventory count for the current organization
    const getInventoryCount = async (): Promise<number> => {
        if (!authStore.organization?.id) {
            return 0
        }

        try {
            const result = await inventoryService.getInventoryCount(authStore.organization.id)
            return result.success && result.data !== undefined ? result.data : 0
        } catch (err) {
            return 0
        }
    }

    // Clear selected inventory
    const clearSelection = () => {
        selectedInventory.value = null
        toast.info('Inventory selection cleared')
    }

    // Initialize inventory state
    const initialize = async () => {
        if (authStore.organization?.id) {
            await fetchOrganizationInventories()
        }
    }

    // Auto-initialize when composable is used
    onMounted(() => {
        if (authStore.isAuthenticated && authStore.organization) {
            initialize()
        }
    })

    return {
        // State
        inventories,
        selectedInventory,
        isLoading,
        error,

        // Computed
        hasInventories,
        isInventorySelected,
        activeInventories,

        // Methods
        fetchOrganizationInventories,
        fetchActiveInventories,
        selectInventory,
        getInventoryById,
        createInventory,
        updateInventory,
        deleteInventory,
        searchInventories,
        checkInventoryExists,
        getInventoryCount,
        clearSelection,
        initialize
    }
}
