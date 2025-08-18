<template>
  <div class="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
    <div class="w-full max-w-md space-y-8">
      <div class="text-center">
        <h1 class="text-4xl font-bold text-gray-900 mb-2">Welcome!</h1>
        <p class="text-gray-600">Let's set up your organization</p>
      </div>
      
      <OrganizationForm @submit="handleOrganizationSubmit" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { useAuthStore } from '@/stores/auth'
import OrganizationForm from './OrganizationForm.vue'

const emit = defineEmits<{
  'organization-created': []
}>()

const authStore = useAuthStore()

const handleOrganizationSubmit = async (form: { 
  name: string; 
  domain: string; 
  description: string; 
  address: string; 
  phone: string; 
  email: string 
}) => {
  const result = await authStore.createOrganization(form)
  if (result.success) {
    emit('organization-created')
  }
}
</script>
