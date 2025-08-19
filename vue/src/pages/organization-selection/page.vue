<template>
  <div class="min-h-screen w-full bg-gray-50 flex items-center justify-center p-8">
    <div class="w-full max-w-6xl">
      <!-- Header -->
      <div class="text-center mb-12">
        <h1 class="text-4xl font-bold text-gray-900 mb-4">Welcome, {{ user?.name }}</h1>
        <p class="text-lg text-gray-600">Please select an organization to continue or create a new one</p>
      </div>

      <!-- Organization Cards Grid -->
      <div class="grid items-center grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-8 mb-12">
        <!-- Create New Organization Card -->
        <div 
          class="bg-white rounded-xl shadow-lg border-2 border-dashed border-gray-300 hover:border-blue-400 hover:shadow-xl transition-all duration-200 cursor-pointer p-8 text-center min-h-[280px] flex flex-col items-center justify-center"
          @click="showCreateDialog = true"
        >
          <div class="w-20 h-20 mx-auto mb-6 bg-blue-100 rounded-full flex items-center justify-center">
            <PlusIcon class="w-10 h-10 text-blue-600" />
          </div>
          <h3 class="text-xl font-semibold text-gray-900 mb-3">Create New Organization</h3>
          <p class="text-gray-600">Start a new business or project</p>
        </div>

        <!-- Existing Organizations -->
        <div 
          v-for="org in organizations" 
          :key="org.id"
          class="bg-white rounded-xl shadow-lg hover:shadow-xl transition-all duration-200 cursor-pointer p-8 border border-gray-200 min-h-[280px] flex flex-col"
          @click="handleOrganizationSelect(org)"
        >
          <div class="flex items-start justify-between mb-6">
            <div class="w-16 h-16 bg-gradient-to-br from-blue-500 to-purple-600 rounded-xl flex items-center justify-center text-white font-bold text-xl">
              {{ org.name.charAt(0).toUpperCase() }}
            </div>
            <Badge 
              :variant="org.status === 'ACTIVE' ? 'default' : 'secondary'"
              class="text-xs"
            >
              {{ org.status }}
            </Badge>
          </div>
          
          <h3 class="text-xl font-semibold text-gray-900 mb-3">{{ org.name }}</h3>
          <p class="text-gray-600 mb-4 flex-grow">{{ org.description || 'No description' }}</p>
          
          <div class="space-y-2 text-sm text-gray-500 mt-auto">
            <div class="flex items-center">
              <GlobeAltIcon class="w-4 h-4 mr-3" />
              {{ org.domain }}
            </div>
            <div class="flex items-center">
              <PhoneIcon class="w-4 h-4 mr-3" />
              {{ org.phone }}
            </div>
          </div>
        </div>
      </div>

      <!-- Loading State -->
      <div v-if="isLoading" class="text-center">
        <div class="inline-flex items-center px-4 py-2 font-semibold leading-6 text-sm shadow rounded-md text-white bg-blue-500 hover:bg-blue-400 transition ease-in-out duration-150 cursor-not-allowed">
          <svg class="animate-spin -ml-1 mr-3 h-5 w-5 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
          </svg>
          Loading organizations...
        </div>
      </div>

      <!-- Error State -->
      <div v-if="error" class="text-center">
        <button 
          @click="fetchUserOrganizations"
          class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-blue-700 bg-blue-100 hover:bg-blue-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
        >
          <ArrowPathIcon class="w-4 h-4 mr-2" />
          Try Again
        </button>
      </div>

      <!-- No Organizations State -->
      <div v-if="!isLoading && !error && organizations.length === 0" class="text-center">
        <div class="inline-flex items-center px-4 py-2 font-semibold leading-6 text-sm shadow rounded-md text-gray-700 bg-gray-100">
          <InformationCircleIcon class="w-5 h-5 mr-2" />
          No organizations found. Create your first organization to get started.
        </div>
      </div>
    </div>

    <!-- Create Organization Dialog -->
    <CreateOrganizationDialog 
      v-model:open="showCreateDialog"
      @organization-created="onOrganizationCreated"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import { useRouter } from 'vue-router'
import { useAuth } from '@/composables/useAuth'
import { useOrganization } from '@/composables/useOrganization'
import { Badge } from '@/components/ui/badge'
import CreateOrganizationDialog from '@/pages/organization-selection/components/CreateOrganizationDialog.vue'
import { 
  PlusIcon, 
  GlobeAltIcon, 
  PhoneIcon, 
  ArrowPathIcon, 
  InformationCircleIcon 
} from '@heroicons/vue/24/outline'

const router = useRouter()
const { user } = useAuth()
const { 
  organizations, 
  isLoading, 
  error, 
  fetchUserOrganizations, 
  selectOrganization 
} = useOrganization()

const showCreateDialog = ref(false)

// Handle organization selection
const handleOrganizationSelect = (org: any) => {
  selectOrganization(org)
  // Redirect to dashboard or main app
  router.push('/dashboard')
}

// Handle newly created organization
const onOrganizationCreated = (org: any) => {
  showCreateDialog.value = false
  // The organization is automatically selected in the composable
  // Redirect to dashboard
  router.push('/dashboard')
}

onMounted(() => {
  fetchUserOrganizations()
})
</script>
