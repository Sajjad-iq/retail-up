<template>
  <Card
    class="inventory-card ratio-square hover:shadow-xl transition-all duration-300 cursor-pointer border-0 bg-gradient-to-br from-card to-muted/50"
  >
    <CardHeader class="pb-4">
      <div class="flex items-start justify-between">
        <div class="flex-1 min-w-0">
          <CardTitle class="text-xl font-bold text-foreground truncate mb-2">
            {{ inventory.name }}
          </CardTitle>

          <!-- Status and Item Count Row -->
          <div class="flex items-center gap-3 mb-3">
            <Badge
              :variant="inventory.isActive ? 'default' : 'secondary'"
              class="px-3 py-1 text-sm font-medium"
            >
              <div class="flex items-center gap-1.5">
                <div
                  :class="[
                    'w-2 h-2 rounded-full',
                    inventory.isActive ? 'bg-green-500' : 'bg-gray-400',
                  ]"
                ></div>
                {{ inventory.isActive ? "Active" : "Inactive" }}
              </div>
            </Badge>

            <Badge
              variant="outline"
              class="px-3 py-1 text-sm font-medium bg-blue-50 dark:bg-blue-950/20 border-blue-200 dark:border-blue-800 text-blue-700 dark:text-blue-300"
            >
              <ArchiveBoxIcon class="w-4 h-4 mr-1.5" />
              {{ inventory.inventoryItems?.length || 0 }} items
            </Badge>
          </div>

          <!-- Description -->
          <div
            v-if="inventory.description"
            class="text-sm text-muted-foreground line-clamp-2 mb-3 leading-relaxed"
          >
            {{ inventory.description }}
          </div>
        </div>

        <!-- Action Menu -->
        <DropdownMenu @click.stop>
          <DropdownMenuTrigger asChild>
            <Button
              variant="ghost"
              size="sm"
              class="h-9 w-9 p-0 hover:bg-accent rounded-full transition-colors"
            >
              <EllipsisVerticalIcon class="h-4 w-4 text-muted-foreground" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end" class="w-48">
            <DropdownMenuItem @click="$emit('select', inventory)" class="cursor-pointer">
              <ArrowPathIcon class="h-4 w-4 mr-2 text-blue-600" />
              <span>Open Inventory</span>
            </DropdownMenuItem>
            <DropdownMenuItem @click="$emit('edit', inventory)" class="cursor-pointer">
              <PencilIcon class="h-4 w-4 mr-2 text-blue-600" />
              <span>Edit Inventory</span>
            </DropdownMenuItem>
            <DropdownMenuItem @click="$emit('view', inventory)" class="cursor-pointer">
              <EyeIcon class="h-4 w-4 mr-2 text-green-600" />
              <span>View Details</span>
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem
              @click="$emit('toggleStatus', inventory)"
              :class="[
                'cursor-pointer',
                inventory.isActive
                  ? 'text-red-600 hover:text-red-700'
                  : 'text-green-600 hover:text-green-700',
              ]"
            >
              <component
                :is="inventory.isActive ? ArchiveBoxIcon : CheckCircleIcon"
                class="h-4 w-4 mr-2"
              />
              <span>{{ inventory.isActive ? "Deactivate" : "Activate" }}</span>
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
    </CardHeader>

    <CardContent class="pt-0">
      <div class="space-y-4">
        <!-- Location Info -->
        <div
          v-if="inventory.location"
          class="flex items-center gap-2 text-sm text-muted-foreground bg-muted px-3 py-2 rounded-lg"
        >
          <MapPinIcon class="h-4 w-4 text-muted-foreground flex-shrink-0" />
          <span class="truncate font-medium">{{ inventory.location }}</span>
        </div>

        <!-- Stats Row -->
        <div class="grid grid-cols-2 gap-4 pt-2">
          <div
            class="text-center p-3 bg-blue-50 dark:bg-blue-950/20 rounded-lg border border-blue-200 dark:border-blue-800"
          >
            <div class="text-2xl font-bold text-blue-600 dark:text-blue-400">
              {{ inventory.inventoryItems?.length || 0 }}
            </div>
            <div class="text-xs text-blue-700 dark:text-blue-300 font-medium">Total Items</div>
          </div>

          <div
            class="text-center p-3 bg-green-50 dark:bg-green-950/20 rounded-lg border border-green-200 dark:border-green-800"
          >
            <div class="text-2xl font-bold text-green-600 dark:text-green-400">
              {{ inventory.inventoryItems?.filter((item) => item.isActive).length || 0 }}
            </div>
            <div class="text-xs text-green-700 dark:text-green-300 font-medium">Active Items</div>
          </div>
        </div>

        <!-- Timestamps -->
        <div
          class="flex items-center justify-between text-xs text-muted-foreground pt-2 border-t border-border"
        >
          <div class="flex items-center gap-1">
            <CalendarIcon class="h-3 w-3" />
            <span>Created {{ formatDate(inventory.createdAt) }}</span>
          </div>
          <div v-if="inventory.updatedAt !== inventory.createdAt" class="flex items-center gap-1">
            <ClockIcon class="h-3 w-3" />
            <span>Updated {{ formatDate(inventory.updatedAt) }}</span>
          </div>
        </div>
      </div>
    </CardContent>
  </Card>
</template>

<script setup lang="ts">
import { formatDate } from "@/lib/utils";
import type { Inventory } from "@/types/global";

// UI Components
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";

// Icons
import {
  PencilIcon,
  EyeIcon,
  ArchiveBoxIcon,
  CheckCircleIcon,
  EllipsisVerticalIcon,
  MapPinIcon,
  CalendarIcon,
  ClockIcon,
  ArrowPathIcon,
} from "@heroicons/vue/24/outline";

// Props
interface Props {
  inventory: Inventory;
}

defineProps<Props>();

// Emits
defineEmits<{
  select: [inventory: Inventory];
  edit: [inventory: Inventory];
  view: [inventory: Inventory];
  toggleStatus: [inventory: Inventory];
}>();
</script>

<style scoped>
.inventory-card {
  width: 300px;
  --card-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);
  --card-shadow-hover: 0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1);
  --card-border: 1px solid hsl(var(--border));
  --card-border-hover: 1px solid hsl(var(--border) / 0.8);

  box-shadow: var(--card-shadow);
  border: var(--card-border);
  aspect-ratio: 1/1;
}

.inventory-card:hover {
  box-shadow: var(--card-shadow-hover);
  border: var(--card-border-hover);
  transform: translateY(-2px);
}

.inventory-card .line-clamp-2 {
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

/* Custom scrollbar for dropdown */
:deep(.inventory-card .dropdown-menu-content) {
  scrollbar-width: thin;
  scrollbar-color: hsl(var(--muted-foreground)) transparent;
}

:deep(.inventory-card .dropdown-menu-content::-webkit-scrollbar) {
  width: 6px;
}

:deep(.inventory-card .dropdown-menu-content::-webkit-scrollbar-track) {
  background: transparent;
}

:deep(.inventory-card .dropdown-menu-content::-webkit-scrollbar-thumb) {
  background-color: hsl(var(--muted-foreground));
  border-radius: 3px;
}

/* Smooth transitions for all interactive elements */
.inventory-card * {
  transition: all 0.2s ease-in-out;
}

/* Enhanced focus states */
.inventory-card:focus-within {
  outline: 2px solid hsl(var(--ring));
  outline-offset: 2px;
}

/* Responsive adjustments */
@media (max-width: 640px) {
  .inventory-card {
    --card-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
    --card-shadow-hover: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1);
  }

  .inventory-card:hover {
    transform: translateY(-1px);
  }
}
</style>
