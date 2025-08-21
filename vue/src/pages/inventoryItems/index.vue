<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-7xl mx-auto">
      <!-- Page Header -->
      <InventoryItemsPageHeader @create="openCreateDialog" />

      <div v-if="route.params.inventoryId" class="mb-4 flex items-center justify-between">
        <div class="flex items-center space-x-2">
          <span class="text-sm text-gray-600">
            Status: {{ isLoading ? "Loading..." : "Ready" }}
          </span>
        </div>
        <button
          @click="refreshItems"
          :disabled="isLoading"
          class="inline-flex items-center px-3 py-2 border border-gray-300 shadow-sm text-sm leading-4 font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
            />
          </svg>
          Refresh
        </button>
      </div>

      <!-- Search and Filter Bar -->
      <div v-if="route.params.inventoryId" class="mb-6">
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

      <!-- Loading State -->
      <InventoryItemsLoadingState v-if="isLoading" />

      <!-- Error State -->
      <InventoryItemsErrorState v-else-if="error" :error="error" @retry="refreshItems" />

      <!-- Items Table -->
      <InventoryItemsTable
        v-else-if="inventoryItemsQuery.data.value"
        :items="inventoryItemsQuery.data.value.data?.content || []"
        :pagination="pagination"
        @edit="editItem"
        @delete="deleteItem"
        @view="viewItemDetails"
        @page-change="handlePageChange"
      />

      <!-- Empty State -->
      <InventoryItemsEmptyState
        v-else-if="route.params.inventoryId"
        :search-query="filters.searchTerm"
        @create="openCreateDialog"
      />

      <!-- Create/Edit Item Dialog -->
      <InventoryItemDialog
        v-model:open="showDialog"
        :item="selectedItem"
        :mode="dialogMode"
        :inventory-id="route.params.inventoryId as string"
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
import { ref, computed } from "vue";
import { useRoute } from "vue-router";
import { useInventoryItems } from "@/composables/useInventoryItems";

// Custom Components
import {
  InventoryItemDialog,
  InventoryItemDetailsDialog,
  InventoryItemsPageHeader,
  InventoryItemsSearchBar,
  InventoryItemsLoadingState,
  InventoryItemsErrorState,
  InventoryItemsTable,
  InventoryItemsEmptyState,
  DeleteConfirmationDialog,
} from "./components";

// Composables
const route = useRoute();
const { useInventoryItemsList, useDeleteInventoryItem } = useInventoryItems();

// ===== REACTIVE STATE =====
const selectedItem = ref<any>(null);
const showDialog = ref(false);
const showDetailsDialog = ref(false);
const showDeleteDialog = ref(false);
const dialogMode = ref<"create" | "edit">("create");
const currentPage = ref(0);
const pageSize = ref(20);

// Filters
const filters = ref({
  searchTerm: "",
  category: "",
  brand: "",
  isActive: "",
});

// Computed filter for API calls
const apiFilters = computed(() => ({
  searchTerm: filters.value.searchTerm,
  category: filters.value.category,
  brand: filters.value.brand,
  isActive: filters.value.isActive === "all" ? undefined : filters.value.isActive === "true",
}));

// Queries
const inventoryItemsQuery = useInventoryItemsList(
  route.params.inventoryId as string,
  apiFilters.value,
  currentPage.value,
  pageSize.value,
  "createdAt",
  "desc",
  !!route.params.inventoryId
);

// ===== COMPUTED =====
const isLoading = computed(() => inventoryItemsQuery.isLoading.value);
const error = computed(() => inventoryItemsQuery.error.value);

const pagination = computed(() => {
  const data = inventoryItemsQuery.data.value?.data;
  if (!data) return null;

  return {
    currentPage: data.page,
    totalPages: data.totalPages,
    totalElements: data.totalElements,
    pageSize: data.size,
    hasNext: data.hasNext,
    hasPrevious: data.hasPrevious,
  };
});

const deleteMutation = useDeleteInventoryItem();

const handleSearch = () => {
  currentPage.value = 0;
  // The query will automatically refetch due to reactive dependencies
};

const clearFilters = () => {
  filters.value = {
    searchTerm: "",
    category: "",
    brand: "",
    isActive: "all",
  };
  currentPage.value = 0;
};

const refreshItems = async () => {
  // Don't refresh if no inventory is selected
  if (!route.params.inventoryId || (route.params.inventoryId as string).length === 0) {
    return;
  }

  // Manually refetch the current query
  try {
    await inventoryItemsQuery.refetch();
  } catch (error) {
    console.error("Error refreshing items:", error);
  }
};

const handlePageChange = (page: number) => {
  currentPage.value = page;
};

const openCreateDialog = () => {
  selectedItem.value = null;
  dialogMode.value = "create";
  showDialog.value = true;
};

const editItem = (item: any) => {
  selectedItem.value = item;
  dialogMode.value = "edit";
  showDialog.value = true;
};

const viewItemDetails = (item: any) => {
  selectedItem.value = item;
  showDetailsDialog.value = true;
};

const deleteItem = (item: any) => {
  selectedItem.value = item;
  showDeleteDialog.value = true;
};

const confirmDelete = async () => {
  if (!selectedItem.value || !route.params.inventoryId) return;

  await deleteMutation.mutateAsync({
    id: selectedItem.value.id,
    inventoryId: route.params.inventoryId as string,
  });

  showDeleteDialog.value = false;
  selectedItem.value = null;
};

const handleDialogSuccess = () => {
  showDialog.value = false;
  // The queries will automatically refetch due to cache invalidation
};
</script>
