<template>
  <div class="space-y-3">
    <!-- Current Organization Display -->
    <div v-if="selectedOrganization" class="space-y-2">
      <div class="flex items-center space-x-3">
        <div class="w-8 h-8 bg-muted rounded-lg flex items-center justify-center text-muted-foreground font-medium text-sm">
          {{ selectedOrganization.name.charAt(0).toUpperCase() }}
        </div>
        <div class="flex-1 min-w-0">
          <p class="text-sm font-medium text-foreground truncate">{{ selectedOrganization.name }}</p>
          <p class="text-xs text-muted-foreground truncate">{{ selectedOrganization.domain }}</p>
        </div>
        
        <!-- Organization Menu -->
        <DropdownMenu>
          <DropdownMenuTrigger as-child>
            <Button variant="ghost" size="sm" class="h-6 w-6 p-0 text-muted-foreground hover:text-foreground">
              <ChevronDownIcon class="h-3 w-3" />
              <span class="sr-only">Open organization menu</span>
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end" class="w-56">
            <DropdownMenuLabel>Current Organization</DropdownMenuLabel>
            <DropdownMenuSeparator />
            
            <!-- Current Organization Info -->
            <div class="px-2 py-1.5">
              <p class="text-sm font-medium">{{ selectedOrganization.name }}</p>
              <p class="text-xs text-muted-foreground">{{ selectedOrganization.domain }}</p>
            </div>
            
            <DropdownMenuSeparator />
            
            <!-- Switch Organization -->
            <DropdownMenuItem @click="goToOrganizationSelection">
              <ArrowPathIcon class="mr-2 h-4 w-4" />
              Switch Organization
            </DropdownMenuItem>
            
            <!-- Create New Organization -->
            <DropdownMenuItem @click="showCreateDialog = true">
              <PlusIcon class="mr-2 h-4 w-4" />
              Create New Organization
            </DropdownMenuItem>
            
            <DropdownMenuSeparator />
            
            <!-- Organization Settings -->
            <DropdownMenuItem @click="goToOrganizationSettings">
              <Cog6ToothIcon class="mr-2 h-4 w-4" />
              Organization Settings
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
    </div>

    <!-- No Organization Selected -->
    <div v-else class="space-y-2">
      <Button 
        variant="outline" 
        size="sm"
        @click="goToOrganizationSelection"
        class="w-full justify-start text-muted-foreground border-muted hover:bg-muted/50"
      >
        <BuildingOfficeIcon class="mr-2 h-4 w-4" />
        Select Organization
      </Button>
    </div>

    <!-- Create Organization Dialog -->
    <CreateOrganizationDialog 
      v-model:open="showCreateDialog"
      @organization-created="onOrganizationCreated"
    />
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { useRouter } from 'vue-router'
import { useOrganization } from '@/composables/useOrganization'
import { Button } from '@/components/ui/button'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import CreateOrganizationDialog from '@/pages/organization/components/CreateOrganizationDialog.vue'
import {
  ChevronDownIcon,
  ArrowPathIcon,
  PlusIcon,
  Cog6ToothIcon,
  BuildingOfficeIcon,
} from '@heroicons/vue/24/outline'

const router = useRouter()
const { selectedOrganization,clearSelection } = useOrganization()

const showCreateDialog = ref(false)

const goToOrganizationSelection = () => {
  clearSelection()
  router.push('/organization-selection')
}

const goToOrganizationSettings = () => {
  router.push('/organization/settings')
}

const onOrganizationCreated = (org: any) => {
  showCreateDialog.value = false
}
</script>
