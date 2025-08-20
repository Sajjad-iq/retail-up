<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-7xl mx-auto">
      
      <!-- Page Header -->
      <InventoryPageHeader @create="openCreateDialog" />

      <!-- Search and Filter Bar -->
      <div class="mb-6">
        <InventorySearchBar
          v-model:search-query="searchQuery"
          :show-active-only="showActiveOnly"
          @search="handleSearch"
          @toggle-active-filter="toggleActiveFilter"
          @refresh="refreshInventories"
        />
      </div>

      <!-- Loading State -->
      <InventoryLoadingState v-if="isLoading" />

      <!-- Error State -->
      <InventoryErrorState
        v-else-if="error"
        :error="error"
        @retry="refreshInventories"
      />

      <!-- Inventories Grid -->
      <InventoryGrid
        v-else-if="filteredInventories.length > 0"
        :inventories="filteredInventories"
        @select="selectInventory"
        @edit="editInventory"
        @view="viewInventoryDetails"
        @toggle-status="toggleInventoryStatus"
      />

      <!-- Empty State -->
      <InventoryEmptyState
        v-else
        :search-query="searchQuery"
        @create="openCreateDialog"
      />

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
import { useInventory } from '@/composables/useInventory'
import type { Inventory } from '@/types/global'

// No UI components needed - all handled by reusable components

// Custom Components
import { 
  InventoryDialog, 
  InventoryDetailsDialog,
  InventoryPageHeader,
  InventorySearchBar,
  InventoryLoadingState,
  InventoryErrorState,
  InventoryGrid,
  InventoryEmptyState
} from './components'

// Composables
const { selectedOrganization } = useOrganization()
const {
  inventories,
  isLoading,
  error,
  fetchOrganizationInventories,
  updateInventory,
  createInventory,
  selectInventory: selectInventoryComposable,
  searchInventories
} = useInventory()

// ===== REACTIVE STATE =====
const searchQuery = ref('')
const showActiveOnly = ref(false)
const showDialog = ref(false)
const showDetailsDialog = ref(false)
const selectedInventory = ref<Inventory | null>(null)
const dialogMode = ref<'create' | 'edit'>('create')

// ===== COMPUTED =====
const filteredInventories = computed(() => {
  let filtered = inventories.value

  // Filter by active status only (search is now handled by the composable)
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
    return
  }
  await fetchOrganizationInventories()
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
const handleSearch = async () => {
  if (searchQuery.value.trim()) {
    await searchInventories(searchQuery.value)
  } else {
    await fetchOrganizationInventories()
  }
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
  // Use the composable to select the inventory
  selectInventoryComposable(inventory)
  // Navigate to inventory details page or items list
  // This could be expanded based on requirements
  viewInventoryDetails(inventory)
}

/**
 * Toggle inventory active status
 */
const toggleInventoryStatus = async (inventory: Inventory) => {
  const success = await updateInventory(inventory.id, {
    isActive: !inventory.isActive
  })
  
  if (success) {
    // The composable handles the success toast and state update
  }
}

/**
 * Handle dialog success
 */
const handleDialogSuccess = (inventory: Inventory) => {
  // The composable handles state updates and success toasts
  showDialog.value = false
}

// ===== WATCHERS =====
watch(selectedOrganization, (newOrg) => {
  if (newOrg) {
    loadInventories()
  }
}, { immediate: true })

// ===== LIFECYCLE HOOKS =====
onMounted(() => {
  if (selectedOrganization.value) {
    loadInventories()
  }
})
</script>
