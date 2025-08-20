<template>
  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
    <InventoryCard
      v-for="inventory in filteredInventories"
      :key="inventory.id"
      :inventory="inventory"
      @select="$emit('select', $event)"
      @edit="$emit('edit', $event)"
      @view="$emit('view', $event)"
      @toggle-status="$emit('toggleStatus', $event)"
    />
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import type { Inventory } from '@/types/global'
import InventoryCard from './InventoryCard.vue'

// Props
interface Props {
  inventories: Inventory[]
  showActiveOnly?: boolean
}

const props = defineProps<Props>()

// Emits
defineEmits<{
  select: [inventory: Inventory]
  edit: [inventory: Inventory]
  view: [inventory: Inventory]
  toggleStatus: [inventory: Inventory]
}>()

// Computed
const filteredInventories = computed(() => {
  let filtered = props.inventories

  // Filter by active status if showActiveOnly is true
  if (props.showActiveOnly) {
    filtered = filtered.filter(inventory => inventory.isActive)
  }

  return filtered
})
</script>
