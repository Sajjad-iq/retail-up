<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-[600px]">
      <DialogHeader>
        <DialogTitle class="flex items-center gap-2">
          <ArchiveBoxIcon class="h-5 w-5 text-blue-600" />
          {{ inventory?.name || 'Inventory Details' }}
        </DialogTitle>
        <DialogDescription>
          Detailed information about this inventory location
        </DialogDescription>
      </DialogHeader>

      <div v-if="inventory" class="space-y-6">
        <!-- Basic Information -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-gray-900">Basic Information</h3>
          
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label class="text-sm font-medium text-gray-500">Name</label>
              <p class="mt-1 text-sm text-gray-900">{{ inventory.name }}</p>
            </div>
            
            <div>
              <label class="text-sm font-medium text-gray-500">Status</label>
              <div class="mt-1">
                <Badge :variant="inventory.isActive ? 'default' : 'secondary'">
                  {{ inventory.isActive ? 'Active' : 'Inactive' }}
                </Badge>
              </div>
            </div>
            
            <div v-if="inventory.description" class="md:col-span-2">
              <label class="text-sm font-medium text-gray-500">Description</label>
              <p class="mt-1 text-sm text-gray-900">{{ inventory.description }}</p>
            </div>
            
            <div v-if="inventory.location" class="md:col-span-2">
              <label class="text-sm font-medium text-gray-500">Location</label>
              <div class="mt-1 flex items-center gap-2 text-sm text-gray-900">
                <MapPinIcon class="h-4 w-4 text-gray-400" />
                {{ inventory.location }}
              </div>
            </div>
          </div>
        </div>

        <!-- Statistics -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-gray-900">Statistics</h3>
          
          <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div class="bg-blue-50 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-blue-600">
                {{ inventory.inventoryItems?.length || 0 }}
              </div>
              <div class="text-sm text-blue-600">Total Items</div>
            </div>
            
            <div class="bg-green-50 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-green-600">
                {{ formatDate(inventory.createdAt) }}
              </div>
              <div class="text-sm text-green-600">Created</div>
            </div>
            
            <div class="bg-purple-50 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-purple-600">
                {{ formatDate(inventory.updatedAt) }}
              </div>
              <div class="text-sm text-purple-600">Last Updated</div>
            </div>
          </div>
        </div>

        <!-- Organization Information -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-gray-900">Organization</h3>
          
          <div class="bg-gray-50 rounded-lg p-4">
            <div class="flex items-center gap-3">
              <BuildingOfficeIcon class="h-5 w-5 text-gray-600" />
              <div>
                <p class="text-sm font-medium text-gray-900">
                  {{ inventory.organization?.name || 'Organization' }}
                </p>
                <p class="text-xs text-gray-500">Organization ID: {{ inventory.organizationId }}</p>
              </div>
            </div>
          </div>
        </div>

        <!-- Action Buttons -->
        <div class="flex items-center justify-end gap-3 pt-4 border-t">
          <Button
            variant="outline"
            @click="$emit('update:open', false)"
          >
            Close
          </Button>
          <Button
            @click="editInventory"
            class="flex items-center gap-2"
          >
            <PencilIcon class="h-4 w-4" />
            Edit Inventory
          </Button>
        </div>
      </div>

      <!-- Loading State -->
      <div v-else class="flex items-center justify-center py-12">
        <div class="text-center">
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
          <p class="mt-2 text-sm text-gray-600">Loading inventory details...</p>
        </div>
      </div>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import { formatDate } from '@/lib/utils'
import type { Inventory } from '@/types/global'

// UI Components
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'

// Icons
import {
  ArchiveBoxIcon,
  MapPinIcon,
  BuildingOfficeIcon,
  PencilIcon,
} from '@heroicons/vue/24/outline'

// Props
interface Props {
  open: boolean
  inventory?: Inventory | null
}

// Emits
interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'edit', inventory: Inventory): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

// ===== METHODS =====

/**
 * Emit edit event to parent component
 */
const editInventory = () => {
  if (props.inventory) {
    emit('edit', props.inventory)
    emit('update:open', false)
  }
}
</script>
