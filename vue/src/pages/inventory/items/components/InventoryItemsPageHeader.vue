<template>
  <div class="flex items-center justify-between mb-8">
    <div>
      <h1 class="text-3xl font-bold text-gray-900">Inventory Items</h1>
      <p class="mt-2 text-gray-600">
        Manage and track your inventory items across all locations
      </p>
    </div>
    
    <Button @click="$emit('create')" class="flex items-center gap-2">
      <PlusIcon class="h-4 w-4" />
      Add Item
    </Button>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { PlusIcon } from '@heroicons/vue/24/outline'

defineEmits<{
  create: []
}>()
</script>
