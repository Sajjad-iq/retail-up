<template>
  <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
    <!-- Total Items Count -->
    <div class="bg-white rounded-lg border p-6">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <div class="w-8 h-8 bg-blue-100 rounded-lg flex items-center justify-center">
            <CubeIcon class="h-5 w-5 text-blue-600" />
          </div>
        </div>
        <div class="ml-4">
          <p class="text-sm font-medium text-gray-500">Total Items</p>
          <div v-if="counts.totalCount.isLoading" class="mt-1">
            <div class="h-6 w-16 bg-gray-200 rounded animate-pulse"></div>
          </div>
          <p v-else class="text-2xl font-semibold text-gray-900">
            {{ counts.totalCount.data?.data || 0 }}
          </p>
        </div>
      </div>
    </div>

    <!-- Active Items Count -->
    <div class="bg-white rounded-lg border p-6">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <div class="w-8 h-8 bg-green-100 rounded-lg flex items-center justify-center">
            <CheckCircleIcon class="h-5 w-5 text-green-600" />
          </div>
        </div>
        <div class="ml-4">
          <p class="text-sm font-medium text-gray-500">Active Items</p>
          <div v-if="counts.activeCount.isLoading" class="mt-1">
            <div class="h-6 w-16 bg-gray-200 rounded animate-pulse"></div>
          </div>
          <p v-else class="text-2xl font-semibold text-gray-900">
            {{ counts.activeCount.data?.data || 0 }}
          </p>
        </div>
      </div>
    </div>

    <!-- Low Stock Items Count -->
    <div class="bg-white rounded-lg border p-6">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <div class="w-8 h-8 bg-yellow-100 rounded-lg flex items-center justify-center">
            <ExclamationTriangleIcon class="h-5 w-5 text-yellow-600" />
          </div>
        </div>
        <div class="ml-4">
          <p class="text-sm font-medium text-gray-500">Low Stock Items</p>
          <p class="text-2xl font-semibold text-gray-900">
            {{ lowStockCount }}
          </p>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { 
  CubeIcon, 
  CheckCircleIcon, 
  ExclamationTriangleIcon 
} from '@heroicons/vue/24/outline'

interface Props {
  counts: {
    totalCount: any
    activeCount: any
    isLoading: boolean
    error: any
  }
}

const props = defineProps<Props>()

// Calculate low stock items (this would need to come from the actual data)
const lowStockCount = computed(() => {
  // For now, return 0 - this would be calculated from actual inventory data
  return 0
})
</script>
