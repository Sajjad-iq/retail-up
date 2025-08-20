<template>
  <div class="mb-8">
    <div class="flex items-center justify-between">
      <div>
        <h1 class="text-3xl font-bold text-gray-900">Inventory Management</h1>
        <p class="mt-2 text-gray-600">Manage your organization's inventories and storage locations</p>
      </div>
      <Button @click="handleCreate" class="flex items-center gap-2">
        <PlusIcon class="h-5 w-5" />
        Create Inventory
      </Button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { PlusIcon } from '@heroicons/vue/24/outline'

// Emits
const emit = defineEmits<{
  create: []
}>()

// Methods
const handleCreate = () => {
  emit('create')
}
</script>
