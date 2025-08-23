<template>
  <div class="min-h-screen bg-background p-8">
    <div class="max-w-7xl mx-auto">
      <!-- Page Header -->
      <InventoryItemsPageHeader @create="openCreateDialog" />

      <div v-if="route.params.inventoryId" class="mb-4 flex items-center justify-between">
        <div class="flex items-center space-x-2">
          <span class="text-sm text-muted-foreground">
            Status: {{ isLoading ? "Loading..." : "Ready" }}
          </span>
        </div>
        <button
          @click="refreshItems"
          :disabled="isLoading"
          class="inline-flex items-center px-3 py-2 border border-border shadow-sm text-sm leading-4 font-medium rounded-md text-foreground bg-card hover:bg-accent focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:opacity-50 disabled:cursor-not-allowed"
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
        :key="`table-${JSON.stringify(apiFilters)}-${currentPage}`"
        :items="tableItems"
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
import { ref, computed, watch, reactive } from "vue";
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
import { toast } from "vue-sonner";
import { queryUtils } from "@/config/query";

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
const filters = reactive({
  searchTerm: "",
  category: "",
  brand: "",
  isActive: "all",
});

// Computed filter for API calls
const apiFilters = computed(() => ({
  searchTerm: filters.searchTerm,
  category: filters.category,
  brand: filters.brand,
  isActive: filters.isActive === "all" ? undefined : filters.isActive === "true" ? true : false,
}));

// Queries - Pass reactive refs to make the query reactive to parameter changes
const inventoryItemsQuery = useInventoryItemsList(
  route.params.inventoryId as string,
  apiFilters,
  currentPage,
  pageSize,
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

// Computed table items to ensure reactivity
const tableItems = computed(() => {
  const items = inventoryItemsQuery.data.value?.data?.content || [];
  return items;
});

const deleteMutation = useDeleteInventoryItem();

const handleSearch = async () => {
  currentPage.value = 0;
  // Invalidate cache and force a refetch when search is triggered
  queryUtils.clearAll();
  try {
    await inventoryItemsQuery.refetch();
  } catch (error) {
    console.error("Search error:", error);
  }
};

const clearFilters = async () => {
  filters.searchTerm = "";
  filters.category = "";
  filters.brand = "";
  filters.isActive = "all";
  currentPage.value = 0;

  // Invalidate cache and force a refetch after clearing filters
  queryUtils.clearAll();
  try {
    await inventoryItemsQuery.refetch();
  } catch (error) {
    console.error("Clear filters error:", error);
  }
};

const refreshItems = async () => {
  // Don't refresh if no inventory is selected
  if (!route.params.inventoryId || (route.params.inventoryId as string).length === 0) {
    return;
  }

  // Invalidate the specific query cache first to ensure fresh data
  queryUtils.clearAll();

  // Manually refetch the current query
  try {
    await inventoryItemsQuery.refetch();
  } catch (error) {
    toast.error("Error refreshing items");
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

const handleDialogSuccess = async () => {
  // Invalidate the specific query cache first to ensure fresh data
  queryUtils.clearAll();

  // Then refetch the current query
  await inventoryItemsQuery.refetch();
  showDialog.value = false;
};
</script>
