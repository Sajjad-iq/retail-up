<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-[600px]">
      <DialogHeader>
        <DialogTitle class="flex items-center gap-2">
          <ArchiveBoxIcon class="h-5 w-5 text-primary" />
          {{ inventory?.name || 'Inventory Details' }}
        </DialogTitle>
        <DialogDescription class="text-muted-foreground">
          Detailed information about this inventory location
        </DialogDescription>
      </DialogHeader>

      <div v-if="inventory" class="space-y-6">
        <!-- Basic Information -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-foreground">Basic Information</h3>
          
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div class="space-y-2">
              <label class="text-sm font-medium text-muted-foreground">Name</label>
              <p class="text-sm text-foreground font-medium">{{ inventory.name }}</p>
            </div>
            
            <div class="space-y-2">
              <label class="text-sm font-medium text-muted-foreground">Status</label>
              <div>
                <Badge :variant="inventory.isActive ? 'default' : 'secondary'">
                  {{ inventory.isActive ? 'Active' : 'Inactive' }}
                </Badge>
              </div>
            </div>
            
            <div v-if="inventory.description" class="md:col-span-2 space-y-2">
              <label class="text-sm font-medium text-muted-foreground">Description</label>
              <p class="text-sm text-foreground bg-muted/50 rounded-md p-3">{{ inventory.description }}</p>
            </div>
            
            <div v-if="inventory.location" class="md:col-span-2 space-y-2">
              <label class="text-sm font-medium text-muted-foreground">Location</label>
              <div class="flex items-center gap-2 text-sm text-foreground bg-muted/50 rounded-md p-3">
                <MapPinIcon class="h-4 w-4 text-muted-foreground" />
                {{ inventory.location }}
              </div>
            </div>
          </div>
        </div>

        <!-- Statistics -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-foreground">Statistics</h3>
          
          <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div class="bg-primary/5 border border-primary/20 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-primary">
                {{ inventory.inventoryItems?.length || 0 }}
              </div>
              <div class="text-sm text-muted-foreground">Total Items</div>
            </div>
            
            <div class="bg-green-500/5 border border-green-500/20 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-green-600">
                {{ formatDate(inventory.createdAt) }}
              </div>
              <div class="text-sm text-muted-foreground">Created</div>
            </div>
            
            <div class="bg-purple-500/5 border border-purple-500/20 rounded-lg p-4 text-center">
              <div class="text-2xl font-bold text-purple-600">
                {{ formatDate(inventory.updatedAt) }}
              </div>
              <div class="text-sm text-muted-foreground">Last Updated</div>
            </div>
          </div>
        </div>

        <!-- Organization Information -->
        <div class="space-y-4">
          <h3 class="text-lg font-semibold text-foreground">Organization</h3>
          
          <div class="bg-muted/50 border border-border rounded-lg p-4">
            <div class="flex items-center gap-3">
              <BuildingOfficeIcon class="h-5 w-5 text-muted-foreground" />
              <div>
                <p class="text-sm font-medium text-foreground">
                  {{ inventory.organization?.name || 'Organization' }}
                </p>
                <p class="text-xs text-muted-foreground">Organization ID: {{ inventory.organizationId }}</p>
              </div>
            </div>
          </div>
        </div>

        <!-- Action Buttons -->
        <div class="flex items-center justify-end gap-3 pt-4 border-t border-border">
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
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto"></div>
          <p class="mt-2 text-sm text-muted-foreground">Loading inventory details...</p>
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
