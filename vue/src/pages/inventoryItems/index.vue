<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-7xl mx-auto">
      
      <!-- Page Header -->
      <InventoryItemsPageHeader @create="openCreateDialog" />

      <!-- Inventory Selection -->
      <div class="mb-6">
        <InventorySelector
          v-model:selected-inventory="selectedInventoryId"
          :inventories="inventories"
          @change="handleInventoryChange"
        />
      </div>

      <!-- Search and Filter Bar -->
      <div v-if="selectedInventoryId" class="mb-6">
        <InventoryItemsSearchBar
          v-model:search-query="filters.searchTerm"
          v-model:category="filters.category"
          v-model:brand="filters.brand"
          v-model:active-only="filters.isActive"
          @search="handleSearch"
          @clear-filters="clearFilters"
          @refresh="refreshItems"
        />
      </div>

      <!-- Item Counts -->
      <div v-if="selectedInventoryId" class="mb-6">
        <InventoryItemsCounts :counts="itemCounts" />
      </div>

      <!-- Loading State -->
      <InventoryItemsLoadingState v-if="isLoading" />

      <!-- Error State -->
      <InventoryItemsErrorState
        v-else-if="error"
        :error="error"
        @retry="refreshItems"
      />

      <!-- Items Table -->
      <InventoryItemsTable
        v-else-if="inventoryItemsQuery.data?.data?.content?.length"
        :items="inventoryItemsQuery.data.data.content"
        :pagination="pagination"
        @edit="editItem"
        @delete="deleteItem"
        @view="viewItemDetails"
        @page-change="handlePageChange"
      />

      <!-- Empty State -->
      <InventoryItemsEmptyState
        v-else-if="selectedInventoryId"
        :search-query="filters.searchTerm"
        @create="openCreateDialog"
      />

      <!-- No Inventory Selected State -->
      <InventoryItemsNoSelectionState
        v-else
        @select-inventory="openInventorySelector"
      />

      <!-- Create/Edit Item Dialog -->
      <InventoryItemDialog
        v-model:open="showDialog"
        :item="selectedItem"
        :mode="dialogMode"
        :inventory-id="selectedInventoryId"
        @success="handleDialogSuccess"
      />

      <!-- Item Details Dialog -->
      <InventoryItemDetailsDialog
        v-model:open="showDetailsDialog"
        :item="selectedItem"
        @edit="editItem"
      />

      <!-- Delete Confirmation Dialog -->
      <DeleteConfirmationDialog
        v-model:open="showDeleteDialog"
        :item-name="selectedItem?.name"
        @confirm="confirmDelete"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import { useOrganization } from '@/composables/useOrganization'
import { useInventory } from '@/composables/useInventory'
import { useInventoryItems } from '@/composables/useInventoryItems'
import type { Inventory } from '@/types/global'
import type { FilterRequest } from '@/services/inventoryItemService'

// Custom Components
import { 
  InventoryItemDialog, 
  InventoryItemDetailsDialog,
  InventoryItemsPageHeader,
  InventorySelector,
  InventoryItemsSearchBar,
  InventoryItemsCounts,
  InventoryItemsLoadingState,
  InventoryItemsErrorState,
  InventoryItemsTable,
  InventoryItemsEmptyState,
  InventoryItemsNoSelectionState,
  DeleteConfirmationDialog
} from './components'

// Composables
const { selectedOrganization } = useOrganization()
const { inventories, fetchOrganizationInventories } = useInventory()
const {
  useInventoryItemsList,
  useItemCounts,
  useCreateInventoryItem,
  useUpdateInventoryItem,
  useDeleteInventoryItem
} = useInventoryItems()

// ===== REACTIVE STATE =====
const selectedInventoryId = ref('')
const selectedItem = ref<any>(null)
const showDialog = ref(false)
const showDetailsDialog = ref(false)
const showDeleteDialog = ref(false)
const dialogMode = ref<'create' | 'edit'>('create')
const currentPage = ref(0)
const pageSize = ref(20)

// Filters
const filters = ref<FilterRequest>({
  searchTerm: '',
  category: '',
  brand: '',
  isActive: undefined
})

// Queries
const inventoryItemsQuery = useInventoryItemsList(
  selectedInventoryId.value,
  filters.value,
  currentPage.value,
  pageSize.value,
  'createdAt',
  'desc',
  computed(() => !!selectedInventoryId.value)
)

const itemCounts = useItemCounts(
  selectedInventoryId.value,
  computed(() => !!selectedInventoryId.value)
)

// Mutations
const createMutation = useCreateInventoryItem()
const updateMutation = useUpdateInventoryItem()
const deleteMutation = useDeleteInventoryItem()

// ===== COMPUTED =====
const isLoading = computed(() => inventoryItemsQuery.isLoading.value)
const error = computed(() => inventoryItemsQuery.error.value)
const pagination = computed(() => {
  const data = inventoryItemsQuery.data.value?.data
  if (!data) return null
  
  return {
    currentPage: data.page,
    totalPages: data.totalPages,
    totalElements: data.totalElements,
    pageSize: data.size,
    hasNext: data.hasNext,
    hasPrevious: data.hasPrevious
  }
})

// ===== METHODS =====
const loadInventories = async () => {
  if (selectedOrganization.value?.id) {
    await fetchOrganizationInventories()
  }
}

const handleInventoryChange = () => {
  currentPage.value = 0
  clearFilters()
  refreshItems()
}

const handleSearch = () => {
  currentPage.value = 0
  // The query will automatically refetch due to reactive dependencies
}

const clearFilters = () => {
  filters.value = {
    searchTerm: '',
    category: '',
    brand: '',
    isActive: undefined
  }
  currentPage.value = 0
}

const refreshItems = () => {
  // The query will automatically refetch due to reactive dependencies
}

const handlePageChange = (page: number) => {
  currentPage.value = page
}

const openCreateDialog = () => {
  selectedItem.value = null
  dialogMode.value = 'create'
  showDialog.value = true
}

const editItem = (item: any) => {
  selectedItem.value = item
  dialogMode.value = 'edit'
  showDialog.value = true
}

const viewItemDetails = (item: any) => {
  selectedItem.value = item
  showDetailsDialog.value = true
}

const deleteItem = (item: any) => {
  selectedItem.value = item
  showDeleteDialog.value = true
}

const confirmDelete = async () => {
  if (!selectedItem.value || !selectedInventoryId.value) return
  
  await deleteMutation.mutateAsync({
    id: selectedItem.value.id,
    inventoryId: selectedInventoryId.value
  })
  
  showDeleteDialog.value = false
  selectedItem.value = null
}

const openInventorySelector = () => {
  // This could open a modal or navigate to inventory selection
  console.log('Open inventory selector')
}

const handleDialogSuccess = () => {
  showDialog.value = false
  // The queries will automatically refetch due to cache invalidation
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
