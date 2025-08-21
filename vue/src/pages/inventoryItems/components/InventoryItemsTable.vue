<template>
  <div class="bg-white rounded-lg border">
    <!-- Table Header -->
    <div class="px-6 py-4 border-b">
      <h3 class="text-lg font-medium text-gray-900">Inventory Items</h3>
    </div>

    <!-- Table -->
    <div class="overflow-x-auto">
      <table class="min-w-full divide-y divide-gray-200">
        <thead class="bg-gray-50">
          <tr>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Item
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              SKU/Barcode
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Category
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Stock
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Price
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Status
            </th>
            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Actions
            </th>
          </tr>
        </thead>
        <tbody class="bg-white divide-y divide-gray-200">
          <tr v-for="item in items" :key="item.id" class="hover:bg-gray-50">
            <!-- Item Name & Description -->
            <td class="px-6 py-4 whitespace-nowrap">
              <div class="flex items-center">
                <div class="flex-shrink-0 h-10 w-10">
                  <div class="h-10 w-10 rounded-lg bg-gray-200 flex items-center justify-center">
                    <CubeIcon class="h-6 w-6 text-gray-600" />
                  </div>
                </div>
                <div class="ml-4">
                  <div class="text-sm font-medium text-gray-900">{{ item.name }}</div>
                  <div v-if="item.description" class="text-sm text-gray-500 truncate max-w-xs">
                    {{ item.description }}
                  </div>
                </div>
              </div>
            </td>

            <!-- SKU/Barcode -->
            <td class="px-6 py-4 whitespace-nowrap">
              <div class="space-y-1">
                <div v-if="item.sku" class="text-sm text-gray-900">
                  <span class="font-medium">SKU:</span> {{ item.sku }}
                </div>
                <div v-if="item.barcode" class="text-sm text-gray-500">
                  <span class="font-medium">Barcode:</span> {{ item.barcode }}
                </div>
              </div>
            </td>

            <!-- Category & Brand -->
            <td class="px-6 py-4 whitespace-nowrap">
              <div class="space-y-1">
                <div v-if="item.category" class="text-sm text-gray-900">
                  {{ item.category }}
                </div>
                <div v-if="item.brand" class="text-sm text-gray-500">
                  {{ item.brand }}
                </div>
              </div>
            </td>

            <!-- Stock Information -->
            <td class="px-6 py-4 whitespace-nowrap">
              <div class="space-y-1">
                <div class="text-sm text-gray-900">
                  <span class="font-medium">{{ item.currentStock }}</span>
                  <span class="text-gray-500"> {{ item.unit }}</span>
                </div>
                <div v-if="item.minimumStock !== undefined" class="text-xs text-gray-500">
                  Min: {{ item.minimumStock }}
                </div>
              </div>
            </td>

            <!-- Price Information -->
            <td class="px-6 py-4 whitespace-nowrap">
              <div class="space-y-1">
                <div class="text-sm text-gray-900">
                  <span class="font-medium">${{ item.sellingPrice.amount }}</span>
                  <span class="text-xs text-gray-500"> {{ item.sellingPrice.currency }}</span>
                </div>
                <div v-if="item.costPrice" class="text-xs text-gray-500">
                  Cost: ${{ item.costPrice.amount }}
                </div>
              </div>
            </td>

            <!-- Status -->
            <td class="px-6 py-4 whitespace-nowrap">
              <Badge :variant="item.isActive ? 'default' : 'secondary'">
                {{ item.isActive ? 'Active' : 'Inactive' }}
              </Badge>
            </td>

            <!-- Actions -->
            <td class="px-6 py-4 whitespace-nowrap text-sm font-medium">
              <div class="flex items-center space-x-2">
                <Button
                  @click="$emit('view', item)"
                  variant="ghost"
                  size="sm"
                  class="h-8 w-8 p-0"
                >
                  <EyeIcon class="h-4 w-4" />
                </Button>
                <Button
                  @click="$emit('edit', item)"
                  variant="ghost"
                  size="sm"
                  class="h-8 w-8 p-0"
                >
                  <PencilIcon class="h-4 w-4" />
                </Button>
                <Button
                  @click="$emit('delete', item)"
                  variant="ghost"
                  size="sm"
                  class="h-8 w-8 p-0 text-red-600 hover:text-red-700"
                >
                  <TrashIcon class="h-4 w-4" />
                </Button>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    </div>

    <!-- Pagination -->
    <div v-if="pagination" class="px-6 py-4 border-t bg-gray-50">
      <div class="flex items-center justify-between">
        <div class="text-sm text-gray-700">
          Showing {{ (pagination.currentPage * pagination.pageSize) + 1 }} to 
          {{ Math.min((pagination.currentPage + 1) * pagination.pageSize, pagination.totalElements) }} of 
          {{ pagination.totalElements }} results
        </div>
        
        <div class="flex items-center space-x-2">
          <Button
            @click="$emit('pageChange', pagination.currentPage - 1)"
            :disabled="!pagination.hasPrevious"
            variant="outline"
            size="sm"
          >
            Previous
          </Button>
          
          <span class="text-sm text-gray-700">
            Page {{ pagination.currentPage + 1 }} of {{ pagination.totalPages }}
          </span>
          
          <Button
            @click="$emit('pageChange', pagination.currentPage + 1)"
            :disabled="!pagination.hasNext"
            variant="outline"
            size="sm"
          >
            Next
          </Button>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { 
  CubeIcon, 
  EyeIcon, 
  PencilIcon, 
  TrashIcon 
} from '@heroicons/vue/24/outline'

interface Props {
  items: any[]
  pagination: {
    currentPage: number
    totalPages: number
    totalElements: number
    pageSize: number
    hasNext: boolean
    hasPrevious: boolean
  } | null
}

interface Emits {
  (e: 'edit', item: any): void
  (e: 'delete', item: any): void
  (e: 'view', item: any): void
  (e: 'pageChange', page: number): void
}

defineProps<Props>()
defineEmits<Emits>()
</script>
