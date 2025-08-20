<template>
  <div class="min-h-screen w-full bg-gray-50 flex items-center justify-center p-8">
    <div class="w-full max-w-6xl">
      <!-- Header -->
      <div class="text-center mb-12">
        <h1 class="text-4xl font-bold text-gray-900 mb-4">Welcome, {{ user?.name }}</h1>
        <p class="text-lg text-gray-600">Please select an organization to continue or create a new one</p>
      </div>

      <!-- Organization Cards Flex Container -->
      <div class="flex flex-wrap justify-center gap-8 mb-12">
        <!-- Create New Organization Card -->
        <CreateOrganizationCard @click="showCreateDialog = true" />

        <!-- Existing Organizations -->
        <OrganizationCard 
          v-for="org in organizations" 
          :key="org.id"
          :organization="org"
          @click="handleOrganizationSelect"
        />
      </div>

      <!-- Loading State -->
      <LoadingState v-if="isLoading" message="Loading organizations..." />

      <!-- Error State -->
      <ErrorState v-if="error" @retry="fetchUserOrganizations" />

      <!-- No Organizations State -->
      <EmptyState 
        v-if="!isLoading && !error && organizations.length === 0" 
        message="No organizations found. Create your first organization to get started." 
      />
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
import { 
  CreateOrganizationDialog,
  LoadingState,
  ErrorState,
  EmptyState,
  CreateOrganizationCard,
  OrganizationCard
} from '../../components'

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
