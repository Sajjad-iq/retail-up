<template>
  <div class="bg-white rounded-lg border p-4">
    <label class="block text-sm font-medium text-gray-700 mb-2">
      Select Inventory
    </label>
    
    <Select v-model="selectedInventory" @update:model-value="handleChange">
      <SelectTrigger class="w-full">
        <SelectValue placeholder="Choose an inventory to view items" />
      </SelectTrigger>
      <SelectContent>
        <SelectItem
          v-for="inventory in inventories"
          :key="inventory.id"
          :value="inventory.id"
        >
          <div class="flex items-center gap-2">
            <div class="w-2 h-2 rounded-full" :class="inventory.isActive ? 'bg-green-500' : 'bg-gray-400'"></div>
            {{ inventory.name }}
            <span class="text-xs text-gray-500 ml-auto">
              {{ inventory.inventoryItems?.length || 0 }} items
            </span>
          </div>
        </SelectItem>
      </SelectContent>
    </Select>
    
    <p v-if="selectedInventory" class="mt-2 text-sm text-gray-600">
      Viewing items from: <span class="font-medium">{{ getSelectedInventoryName() }}</span>
    </p>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import type { Inventory } from '@/types/global'

interface Props {
  selectedInventory: string
  inventories: Inventory[]
}

interface Emits {
  (e: 'update:selectedInventory', value: string): void
  (e: 'change'): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

const selectedInventory = computed({
  get: () => props.selectedInventory,
  set: (value) => emit('update:selectedInventory', value)
})

const handleChange = () => {
  emit('change')
}

const getSelectedInventoryName = () => {
  const inventory = props.inventories.find(inv => inv.id === props.selectedInventory)
  return inventory?.name || 'Unknown'
}
</script>
