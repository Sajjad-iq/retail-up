<template>
  <div class="text-center py-12">
    <ArchiveBoxIcon class="h-16 w-16 text-gray-400 mx-auto mb-4" />
    <h3 class="text-lg font-medium text-gray-900 mb-2">No Inventories Found</h3>
    <p class="text-gray-600 mb-6">
      {{ searchQuery ? 'No inventories match your search.' : 'Get started by creating your first inventory.' }}
    </p>
    <Button @click="handleCreate" variant="outline">
      Create Inventory
    </Button>
  </div>
</template>

<script setup lang="ts">
// UI Components
import { Button } from '@/components/ui/button'

// Icons
import { ArchiveBoxIcon } from '@heroicons/vue/24/outline'

// Props
interface Props {
  searchQuery: string
}

defineProps<Props>()

// Emits
const emit = defineEmits<{
  create: []
}>()

// Methods
const handleCreate = () => {
  emit('create')
}
</script>
