<template>
  <div class="bg-white rounded-lg border p-8">
    <div class="text-center flex flex-col items-center">
      <div class="mx-auto h-12 w-12 text-gray-400">
        <CubeIcon class="h-12 w-12" />
      </div>
      <h3 class="mt-2 text-sm font-medium text-gray-900">
        {{ searchQuery ? 'No items found' : 'No inventory items yet' }}
      </h3>
      <p class="mt-1 text-sm text-gray-500">
        {{ searchQuery 
          ? `No items match your search criteria "${searchQuery}". Try adjusting your filters.`
          : 'Get started by creating your first inventory item.'
        }}
      </p>
      <div class="mt-6">
        <Button @click="$emit('create')" class="flex items-center gap-2">
          <PlusIcon class="h-4 w-4" />
          {{ searchQuery ? 'Clear filters' : 'Add your first item' }}
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { CubeIcon, PlusIcon } from '@heroicons/vue/24/outline'

interface Props {
  searchQuery: string
}

interface Emits {
  (e: 'create'): void
}

defineProps<Props>()
defineEmits<Emits>()
</script>
