<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-7xl mx-auto">
      
      <!-- Page Header -->
      <div class="mb-8">
        <div class="flex items-center justify-between">
          <div>
            <h1 class="text-3xl font-bold text-gray-900">Inventory Management</h1>
            <p class="mt-2 text-gray-600">Manage your organization's inventories and storage locations</p>
          </div>
          <Button @click="openCreateDialog" class="flex items-center gap-2">
            <PlusIcon class="h-5 w-5" />
            Create Inventory
          </Button>
        </div>
      </div>

      <!-- Search and Filter Bar -->
      <div class="mb-6 bg-white rounded-lg shadow-sm border p-4">
        <div class="flex items-center gap-4">
          <div class="flex-1">
            <Input
              v-model="searchQuery"
              placeholder="Search inventories by name..."
              class="max-w-md"
              @input="handleSearch"
            />
          </div>
          <div class="flex items-center gap-2">
            <Button
              variant="outline"
              :class="{ 'bg-blue-50 border-blue-200': showActiveOnly }"
              @click="toggleActiveFilter"
            >
              <CheckCircleIcon class="h-4 w-4 mr-2" />
              Active Only
            </Button>
            <Button variant="outline" @click="refreshInventories">
              <ArrowPathIcon class="h-4 w-4 mr-2" />
              Refresh
            </Button>
          </div>
        </div>
      </div>

      <!-- Loading State -->
      <div v-if="isLoading" class="flex items-center justify-center py-12">
        <div class="text-center">
          <div class="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p class="mt-4 text-gray-600">Loading inventories...</p>
        </div>
      </div>

      <!-- Error State -->
      <div v-else-if="error" class="bg-red-50 border border-red-200 rounded-lg p-6">
        <div class="flex items-center gap-3">
          <ExclamationTriangleIcon class="h-6 w-6 text-red-600" />
          <div>
            <h3 class="text-lg font-medium text-red-800">Error Loading Inventories</h3>
            <p class="text-red-700 mt-1">{{ error }}</p>
          </div>
        </div>
        <Button @click="refreshInventories" variant="outline" class="mt-4">
          Try Again
        </Button>
      </div>

      <!-- Inventories Grid -->
      <div v-else-if="filteredInventories.length > 0" class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        <Card
          v-for="inventory in filteredInventories"
          :key="inventory.id"
          class="hover:shadow-lg transition-shadow duration-200 cursor-pointer"
          @click="selectInventory(inventory)"
        >
          <CardHeader class="pb-3">
            <div class="flex items-start justify-between">
              <div class="flex-1">
                <CardTitle class="text-lg font-semibold text-gray-900 truncate">
                  {{ inventory.name }}
                </CardTitle>
                <div class="flex items-center gap-2 mt-1">
                  <Badge :variant="inventory.isActive ? 'default' : 'secondary'">
                    {{ inventory.isActive ? 'Active' : 'Inactive' }}
                  </Badge>
                  <Badge variant="outline" class="text-xs">
                    {{ inventory.inventoryItems?.length || 0 }} items
                  </Badge>
                </div>
              </div>
              <DropdownMenu>
                <DropdownMenuTrigger asChild>
                  <Button variant="ghost" size="sm" class="h-8 w-8 p-0">
                    <EllipsisVerticalIcon class="h-4 w-4" />
                  </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent align="end">
                  <DropdownMenuItem @click="editInventory(inventory)">
                    <PencilIcon class="h-4 w-4 mr-2" />
                    Edit
                  </DropdownMenuItem>
                  <DropdownMenuItem @click="viewInventoryDetails(inventory)">
                    <EyeIcon class="h-4 w-4 mr-2" />
                    View Details
                  </DropdownMenuItem>
                  <DropdownMenuSeparator />
                  <DropdownMenuItem 
                    @click="toggleInventoryStatus(inventory)"
                    :class="inventory.isActive ? 'text-red-600' : 'text-green-600'"
                  >
                    <component 
                      :is="inventory.isActive ? ArchiveBoxIcon : CheckCircleIcon" 
                      class="h-4 w-4 mr-2" 
                    />
                    {{ inventory.isActive ? 'Deactivate' : 'Activate' }}
                  </DropdownMenuItem>
                </DropdownMenuContent>
              </DropdownMenu>
            </div>
          </CardHeader>
          
          <CardContent class="pt-0">
            <div class="space-y-3">
              <div v-if="inventory.description" class="text-sm text-gray-600 line-clamp-2">
                {{ inventory.description }}
              </div>
              
              <div v-if="inventory.location" class="flex items-center gap-2 text-sm text-gray-500">
                <MapPinIcon class="h-4 w-4" />
                <span class="truncate">{{ inventory.location }}</span>
              </div>
              
              <div class="flex items-center justify-between text-xs text-gray-400">
                <span>Created {{ formatDate(inventory.createdAt) }}</span>
                <span v-if="inventory.updatedAt !== inventory.createdAt">
                  Updated {{ formatDate(inventory.updatedAt) }}
                </span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <!-- Empty State -->
      <div v-else class="text-center py-12">
        <ArchiveBoxIcon class="h-16 w-16 text-gray-400 mx-auto mb-4" />
        <h3 class="text-lg font-medium text-gray-900 mb-2">No Inventories Found</h3>
        <p class="text-gray-600 mb-6">
          {{ searchQuery ? 'No inventories match your search.' : 'Get started by creating your first inventory.' }}
        </p>
        <Button @click="openCreateDialog" variant="outline">
          Create Inventory
        </Button>
      </div>

      <!-- Create/Edit Inventory Dialog -->
      <InventoryDialog
        v-model:open="showDialog"
        :inventory="selectedInventory"
        :mode="dialogMode"
        @success="handleDialogSuccess"
      />

      <!-- Inventory Details Dialog -->
      <InventoryDetailsDialog
        v-model:open="showDetailsDialog"
        :inventory="selectedInventory"
        @edit="editInventory"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import { useOrganization } from '@/composables/useOrganization'
import { inventoryService } from '@/services/inventoryService'
import { toast } from 'vue-sonner'
import { formatDate } from '@/lib/utils'
import type { Inventory } from '@/types/global'

// UI Components
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'

// Icons
import {
  PlusIcon,
  PencilIcon,
  EyeIcon,
  ArchiveBoxIcon,
  CheckCircleIcon,
  ExclamationTriangleIcon,
  ArrowPathIcon,
  EllipsisVerticalIcon,
  MapPinIcon,
} from '@heroicons/vue/24/outline'

// Custom Components
import { InventoryDialog, InventoryDetailsDialog } from './components'

// Composables
const { selectedOrganization } = useOrganization()

// ===== REACTIVE STATE =====
const inventories = ref<Inventory[]>([])
const isLoading = ref(false)
const error = ref<string | null>(null)
const searchQuery = ref('')
const showActiveOnly = ref(false)
const showDialog = ref(false)
const showDetailsDialog = ref(false)
const selectedInventory = ref<Inventory | null>(null)
const dialogMode = ref<'create' | 'edit'>('create')

// ===== COMPUTED =====
const filteredInventories = computed(() => {
  let filtered = inventories.value

  // Filter by search query
  if (searchQuery.value.trim()) {
    filtered = filtered.filter(inventory =>
      inventory.name.toLowerCase().includes(searchQuery.value.toLowerCase()) ||
      inventory.description?.toLowerCase().includes(searchQuery.value.toLowerCase()) ||
      inventory.location?.toLowerCase().includes(searchQuery.value.toLowerCase())
    )
  }

  // Filter by active status
  if (showActiveOnly.value) {
    filtered = filtered.filter(inventory => inventory.isActive)
  }

  return filtered
})

// ===== METHODS =====

/**
 * Load inventories for the selected organization
 */
const loadInventories = async () => {
  if (!selectedOrganization.value?.id) {
    error.value = 'No organization selected'
    return
  }

  isLoading.value = true
  error.value = null

  try {
    const result = await inventoryService.getInventoriesByOrganization(selectedOrganization.value.id)
    
    if (result.success && result.data) {
      inventories.value = result.data
    } else {
      error.value = result.error || 'Failed to load inventories'
    }
  } catch (err) {
    error.value = 'An error occurred while loading inventories'
  } finally {
    isLoading.value = false
  }
}

/**
 * Refresh inventories data
 */
const refreshInventories = () => {
  loadInventories()
}

/**
 * Handle search input
 */
const handleSearch = () => {
  // Search is handled by computed property
  // Could add debouncing here if needed
}

/**
 * Toggle active filter
 */
const toggleActiveFilter = () => {
  showActiveOnly.value = !showActiveOnly.value
}

/**
 * Open create inventory dialog
 */
const openCreateDialog = () => {
  selectedInventory.value = null
  dialogMode.value = 'create'
  showDialog.value = true
}

/**
 * Edit selected inventory
 */
const editInventory = (inventory: Inventory) => {
  selectedInventory.value = inventory
  dialogMode.value = 'edit'
  showDialog.value = true
}

/**
 * View inventory details
 */
const viewInventoryDetails = (inventory: Inventory) => {
  selectedInventory.value = inventory
  showDetailsDialog.value = true
}

/**
 * Select inventory for navigation
 */
const selectInventory = (inventory: Inventory) => {
  // Navigate to inventory details page or items list
  // This could be expanded based on requirements
  viewInventoryDetails(inventory)
}

/**
 * Toggle inventory active status
 */
const toggleInventoryStatus = async (inventory: Inventory) => {
  try {
    const result = await inventoryService.updateInventory(inventory.id, {
      isActive: !inventory.isActive
    })

    if (result.success && result.data) {
      // Update local state
      const index = inventories.value.findIndex(i => i.id === inventory.id)
      if (index !== -1) {
        inventories.value[index] = result.data
      }
      
      toast.success(`Inventory ${inventory.isActive ? 'deactivated' : 'activated'} successfully`)
    } else {
      toast.error(result.error || 'Failed to update inventory status')
    }
  } catch (err) {
    toast.error('An error occurred while updating inventory status')
  }
}

/**
 * Handle dialog success
 */
const handleDialogSuccess = (inventory: Inventory) => {
  if (dialogMode.value === 'create') {
    // Add new inventory to the list
    inventories.value.unshift(inventory)
    toast.success('Inventory created successfully')
  } else {
    // Update existing inventory in the list
    const index = inventories.value.findIndex(i => i.id === inventory.id)
    if (index !== -1) {
      inventories.value[index] = inventory
    }
    toast.success('Inventory updated successfully')
  }
  
  showDialog.value = false
}

// ===== WATCHERS =====
watch(selectedOrganization, (newOrg) => {
  if (newOrg) {
    loadInventories()
  } else {
    inventories.value = []
    error.value = 'No organization selected'
  }
}, { immediate: true })

// ===== LIFECYCLE HOOKS =====
onMounted(() => {
  if (selectedOrganization.value) {
    loadInventories()
  }
})
</script>
