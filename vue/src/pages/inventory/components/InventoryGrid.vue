<template>
  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
    <InventoryCard
      v-for="inventory in inventories"
      :key="inventory.id"
      :inventory="inventory"
      @select="$emit('select', $event)"
      @edit="$emit('edit', $event)"
      @view="$emit('view', $event)"
      @toggleStatus="$emit('toggleStatus', $event)"
    />
  </div>
</template>

<script setup lang="ts">
import type { Inventory } from '@/types/global'
import InventoryCard from './InventoryCard.vue'

// Props
interface Props {
  inventories: Inventory[]
}

defineProps<Props>()

// Emits
defineEmits<{
  select: [inventory: Inventory]
  edit: [inventory: Inventory]
  view: [inventory: Inventory]
  toggleStatus: [inventory: Inventory]
}>()
</script>
