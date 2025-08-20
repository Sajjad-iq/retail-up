<template>
  <div class="bg-white rounded-lg shadow-sm border p-4">
    <div class="flex items-center gap-4">
      <div class="flex-1">
        <Input
          :model-value="searchQuery"
          placeholder="Search inventories by name..."
          class="max-w-md"
          @input="$emit('update:searchQuery', ($event.target as HTMLInputElement).value)"
          @keyup.enter="$emit('search')"
        />
      </div>
      <div class="flex items-center gap-2">
        <Button
          variant="outline"
          :class="{ 'bg-blue-50 border-blue-200': showActiveOnly }"
          @click="$emit('toggleActiveFilter')"
        >
          <CheckCircleIcon class="h-4 w-4 mr-2" />
          Active Only
        </Button>
        <Button variant="outline" @click="$emit('refresh')">
          <ArrowPathIcon class="h-4 w-4 mr-2" />
          Refresh
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// UI Components
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

// Icons
import {
  CheckCircleIcon,
  ArrowPathIcon,
} from '@heroicons/vue/24/outline'

// Props
interface Props {
  searchQuery: string
  showActiveOnly: boolean
}

defineProps<Props>()

// Emits
defineEmits<{
  'update:searchQuery': [value: string]
  search: []
  toggleActiveFilter: []
  refresh: []
}>()
</script>
