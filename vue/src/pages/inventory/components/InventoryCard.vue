<template>
  <Card
    class="hover:shadow-lg transition-shadow duration-200 cursor-pointer"
    @click="$emit('select', inventory)"
  >
    <CardHeader class="pb-3">
      <div class="flex items-start justify-between">
        <div class="flex-1">
          <CardTitle class="text-lg font-semibold text-gray-900 truncate">
            {{ inventory.name }}
          </CardTitle>
          <div class="flex items-center gap-2 mt-1">
            <Badge :variant="inventory.isActive ? 'default' : 'secondary'">
              {{ inventory.isActive ? 'Active' : 'Inactive' }}
            </Badge>
            <Badge variant="outline" class="text-xs">
              {{ inventory.inventoryItems?.length || 0 }} items
            </Badge>
          </div>
        </div>
        <DropdownMenu @click.stop>
          <DropdownMenuTrigger asChild>
            <Button variant="ghost" size="sm" class="h-8 w-8 p-0">
              <EllipsisVerticalIcon class="h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            <DropdownMenuItem @click="$emit('edit', inventory)">
              <PencilIcon class="h-4 w-4 mr-2" />
              Edit
            </DropdownMenuItem>
            <DropdownMenuItem @click="$emit('view', inventory)">
              <EyeIcon class="h-4 w-4 mr-2" />
              View Details
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem 
              @click="$emit('toggleStatus', inventory)"
              :class="inventory.isActive ? 'text-red-600' : 'text-green-600'"
            >
              <component 
                :is="inventory.isActive ? ArchiveBoxIcon : CheckCircleIcon" 
                class="h-4 w-4 mr-2" 
              />
              {{ inventory.isActive ? 'Deactivate' : 'Activate' }}
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
    </CardHeader>
    
    <CardContent class="pt-0">
      <div class="space-y-3">
        <div v-if="inventory.description" class="text-sm text-gray-600 line-clamp-2">
          {{ inventory.description }}
        </div>
        
        <div v-if="inventory.location" class="flex items-center gap-2 text-sm text-gray-500">
          <MapPinIcon class="h-4 w-4" />
          <span class="truncate">{{ inventory.location }}</span>
        </div>
        
        <div class="flex items-center justify-between text-xs text-gray-400">
          <span>Created {{ formatDate(inventory.createdAt) }}</span>
          <span v-if="inventory.updatedAt !== inventory.createdAt">
            Updated {{ formatDate(inventory.updatedAt) }}
          </span>
        </div>
      </div>
    </CardContent>
  </Card>
</template>

<script setup lang="ts">
import { formatDate } from '@/lib/utils'
import type { Inventory } from '@/types/global'

// UI Components
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'

// Icons
import {
  PencilIcon,
  EyeIcon,
  ArchiveBoxIcon,
  CheckCircleIcon,
  EllipsisVerticalIcon,
  MapPinIcon,
} from '@heroicons/vue/24/outline'

// Props
interface Props {
  inventory: Inventory
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
