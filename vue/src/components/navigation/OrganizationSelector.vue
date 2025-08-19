<template>
  <div class="flex items-center space-x-2">
    <!-- Current Organization Display -->
    <div v-if="selectedOrganization" class="flex items-center space-x-2">
      <div class="w-8 h-8 bg-gradient-to-br from-blue-500 to-purple-600 rounded-lg flex items-center justify-center text-white font-bold text-sm">
        {{ selectedOrganization.name.charAt(0).toUpperCase() }}
      </div>
      <div class="hidden md:block">
        <p class="text-sm font-medium text-gray-900">{{ selectedOrganization.name }}</p>
        <p class="text-xs text-gray-500">{{ selectedOrganization.domain }}</p>
      </div>
      
      <!-- Organization Menu -->
      <DropdownMenu>
        <DropdownMenuTrigger as-child>
          <Button variant="ghost" size="sm" class="h-8 w-8 p-0">
            <ChevronDownIcon class="h-4 w-4" />
            <span class="sr-only">Open organization menu</span>
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end" class="w-56">
          <DropdownMenuLabel>Current Organization</DropdownMenuLabel>
          <DropdownMenuSeparator />
          
          <!-- Current Organization Info -->
          <div class="px-2 py-1.5">
            <p class="text-sm font-medium">{{ selectedOrganization.name }}</p>
            <p class="text-xs text-gray-500">{{ selectedOrganization.domain }}</p>
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

    <!-- No Organization Selected -->
    <div v-else class="flex items-center space-x-2">
      <Button 
        variant="outline" 
        size="sm"
        @click="goToOrganizationSelection"
        class="text-orange-600 border-orange-200 hover:bg-orange-50"
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
import CreateOrganizationDialog from '@/pages/organization-selection/components/CreateOrganizationDialog.vue'
import {
  ChevronDownIcon,
  ArrowPathIcon,
  PlusIcon,
  Cog6ToothIcon,
  BuildingOfficeIcon,
} from '@heroicons/vue/24/outline'

const router = useRouter()
const { selectedOrganization, createOrganization } = useOrganization()

const showCreateDialog = ref(false)

const goToOrganizationSelection = () => {
  router.push('/organization-selection')
}

const goToOrganizationSettings = () => {
  router.push('/organization')
}

const onOrganizationCreated = (org: any) => {
  showCreateDialog.value = false
  // The organization is automatically selected in the composable
}
</script>
