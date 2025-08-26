<template>
  <div class="bg-card rounded-lg border p-4">
    <div class="grid grid-cols-1 md:grid-cols-4 gap-4">
      <!-- Search Input -->
      <div>
        <label class="block text-sm font-medium text-foreground mb-1">Search</label>
        <Input
          v-model="searchQuery"
          placeholder="Search by name, barcode, or product code"
          class="w-full"
          @keyup.enter="handleSearch"
        />
      </div>

      <!-- Category Filter -->
      <div>
        <label class="block text-sm font-medium text-foreground mb-1">Category</label>
        <Input v-model="category" placeholder="Filter by category" class="w-full" />
      </div>

      <!-- Brand Filter -->
      <div>
        <label class="block text-sm font-medium text-foreground mb-1">Brand</label>
        <Input v-model="brand" placeholder="Filter by brand" class="w-full" />
      </div>

      <!-- Active Status Filter -->
      <div>
        <label class="block text-sm font-medium text-foreground mb-1">Status</label>
        <Select v-model="activeOnly">
          <SelectTrigger class="w-full">
            <SelectValue placeholder="All items" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All items</SelectItem>
            <SelectItem value="true">Active only</SelectItem>
            <SelectItem value="false">Inactive only</SelectItem>
          </SelectContent>
        </Select>
      </div>
    </div>

    <!-- Action Buttons -->
    <div class="flex items-center justify-between mt-4">
      <div class="flex items-center gap-2">
        <Button @click="handleSearch" variant="default" class="flex items-center gap-2">
          <MagnifyingGlassIcon class="h-4 w-4" />
          Search
        </Button>
        <Button @click="handleClearFilters" variant="outline" class="flex items-center gap-2">
          <XMarkIcon class="h-4 w-4" />
          Clear
        </Button>
      </div>

      <Button @click="$emit('refresh')" variant="ghost" class="flex items-center gap-2">
        <ArrowPathIcon class="h-4 w-4" />
        Refresh
      </Button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, watch } from "vue";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { MagnifyingGlassIcon, XMarkIcon, ArrowPathIcon } from "@heroicons/vue/24/outline";
import { debounce } from "@/lib/utils";

export interface SearchFilters {
  searchTerm: string;
  category: string;
  brand: string;
  isActive: string;
}

interface Emits {
  (e: "filters-changed", filters: SearchFilters): void;
  (e: "search", filters: SearchFilters): void;
  (e: "clear-filters"): void;
  (e: "refresh"): void;
}

const emit = defineEmits<Emits>();

// Internal filter state
const filters = ref<SearchFilters>({
  searchTerm: "",
  category: "",
  brand: "",
  isActive: "all",
});

// Computed properties for v-model
const searchQuery = computed({
  get: () => filters.value.searchTerm,
  set: (value) => {
    filters.value.searchTerm = value;
    debouncedFiltersChanged();
  },
});

const category = computed({
  get: () => filters.value.category,
  set: (value) => {
    filters.value.category = value;
    debouncedFiltersChanged();
  },
});

const brand = computed({
  get: () => filters.value.brand,
  set: (value) => {
    filters.value.brand = value;
    debouncedFiltersChanged();
  },
});

const activeOnly = computed({
  get: () => filters.value.isActive,
  set: (value) => {
    filters.value.isActive = value;
    debouncedFiltersChanged();
  },
});

// Debounced function to emit filter changes
const debouncedFiltersChanged = debounce(() => {
  emit("filters-changed", { ...filters.value });
}, 500);

// Handle search action
const handleSearch = () => {
  emit("search", { ...filters.value });
};

// Handle clear filters
const handleClearFilters = () => {
  filters.value = {
    searchTerm: "",
    category: "",
    brand: "",
    isActive: "all",
  };
  emit("clear-filters");
  emit("filters-changed", { ...filters.value });
};

// Expose current filters for parent components
defineExpose({
  filters: computed(() => filters.value),
  clearFilters: handleClearFilters,
});
</script>
