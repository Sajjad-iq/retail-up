<template>
  <div class="mb-8">
    <div class="flex items-center justify-between">
      <div>
        <h1 class="text-3xl font-bold text-gray-900">Inventory Management</h1>
        <p class="mt-2 text-gray-600">Manage your organization's inventories and storage locations</p>
      </div>
      <Button @click="$emit('create')" class="flex items-center gap-2">
        <PlusIcon class="h-5 w-5" />
        Create Inventory
      </Button>
    </div>
  </div>
</template>

<script setup lang="ts">
// UI Components
import { Button } from '@/components/ui/button'

// Icons
import { PlusIcon } from '@heroicons/vue/24/outline'

// Emits
defineEmits<{
  create: []
}>()
</script>
