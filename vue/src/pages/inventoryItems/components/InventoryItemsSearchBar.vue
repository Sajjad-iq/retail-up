<template>
  <div class="bg-white rounded-lg border p-4">
    <div class="grid grid-cols-1 md:grid-cols-4 gap-4">
      <!-- Search Input -->
      <div>
        <label class="block text-sm font-medium text-gray-700 mb-1">Search</label>
        <Input
          v-model="searchQuery"
          placeholder="Search by name, SKU, or barcode"
          class="w-full"
          @keyup.enter="$emit('search')"
        />
      </div>

      <!-- Category Filter -->
      <div>
        <label class="block text-sm font-medium text-gray-700 mb-1">Category</label>
        <Input v-model="category" placeholder="Filter by category" class="w-full" />
      </div>

      <!-- Brand Filter -->
      <div>
        <label class="block text-sm font-medium text-gray-700 mb-1">Brand</label>
        <Input v-model="brand" placeholder="Filter by brand" class="w-full" />
      </div>

      <!-- Active Status Filter -->
      <div>
        <label class="block text-sm font-medium text-gray-700 mb-1">Status</label>
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
        <Button @click="$emit('search')" variant="default" class="flex items-center gap-2">
          <MagnifyingGlassIcon class="h-4 w-4" />
          Search
        </Button>
        <Button @click="$emit('clear-filters')" variant="outline" class="flex items-center gap-2">
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
import { computed } from "vue";
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

interface Props {
  searchQuery: string;
  category: string;
  brand: string;
  activeOnly: string;
}

interface Emits {
  (e: "update:searchQuery", value: string): void;
  (e: "update:category", value: string): void;
  (e: "update:brand", value: string): void;
  (e: "update:activeOnly", value: string): void;
  (e: "search"): void;
  (e: "clear-filters"): void;
  (e: "refresh"): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const searchQuery = computed({
  get: () => props.searchQuery,
  set: (value) => emit("update:searchQuery", value),
});

const category = computed({
  get: () => props.category,
  set: (value) => emit("update:category", value),
});

const brand = computed({
  get: () => props.brand,
  set: (value) => emit("update:brand", value),
});

const activeOnly = computed({
  get: () => props.activeOnly,
  set: (value) => emit("update:activeOnly", value),
});
</script>
