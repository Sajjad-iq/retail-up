<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-7xl mx-auto">
      
      <!-- Page Header -->
      <InventoryPageHeader @create="openCreateDialog" />

      <!-- Search and Filter Bar -->
      <div class="mb-6">
        <InventorySearchBar
          v-model:search-query="searchQuery"
          v-model:show-active-only="showActiveOnly"
          @search="handleSearch"
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
        v-else-if="inventories.length > 0"
        :inventories="inventories"
        :show-active-only="showActiveOnly"
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
import { ref, onMounted, watch } from 'vue'
import { useOrganization } from '@/composables/useOrganization'
import { useInventory } from '@/composables/useInventory'
import type { Inventory } from '@/types/global'

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

// ===== METHODS =====
const loadInventories = async () => {
  if (selectedOrganization.value?.id) {
    await fetchOrganizationInventories()
  }
}

const refreshInventories = () => loadInventories()

const handleSearch = async () => {
  if (searchQuery.value.trim()) {
    await searchInventories(searchQuery.value)
  } else {
    await fetchOrganizationInventories()
  }
}

const openCreateDialog = () => {
  selectedInventory.value = null
  dialogMode.value = 'create'
  showDialog.value = true
}

const editInventory = (inventory: Inventory) => {
  selectedInventory.value = inventory
  dialogMode.value = 'edit'
  showDialog.value = true
}

const viewInventoryDetails = (inventory: Inventory) => {
  selectedInventory.value = inventory
  showDetailsDialog.value = true
}

const selectInventory = (inventory: Inventory) => {
  selectInventoryComposable(inventory)
}

const toggleInventoryStatus = async (inventory: Inventory) => {
  await updateInventory(inventory.id, { isActive: !inventory.isActive })
}

const handleDialogSuccess = () => {
  showDialog.value = false
  if (selectedOrganization.value?.id) {
    fetchOrganizationInventories()
  }
}

// ===== WATCHERS =====
watch(selectedOrganization, (newOrg) => {
  if (newOrg) loadInventories()
}, { immediate: true })

// ===== LIFECYCLE HOOKS =====
onMounted(() => {
  if (selectedOrganization.value) loadInventories()
})
</script>
