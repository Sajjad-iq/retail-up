<template>
  <div class="bg-white rounded-lg border p-8">
    <div class="text-center">
      <div class="mx-auto h-12 w-12 text-gray-400">
        <BuildingStorefrontIcon class="h-12 w-12" />
      </div>
      <h3 class="mt-2 text-sm font-medium text-gray-900">Select an inventory</h3>
      <p class="mt-1 text-sm text-gray-500">
        Choose an inventory from the dropdown above to view and manage its items.
      </p>
      <div class="mt-6">
        <Button @click="$emit('selectInventory')" variant="outline">
          <BuildingStorefrontIcon class="h-4 w-4 mr-2" />
          Select inventory
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { BuildingStorefrontIcon } from '@heroicons/vue/24/outline'

interface Emits {
  (e: 'selectInventory'): void
}

defineEmits<Emits>()
</script>
