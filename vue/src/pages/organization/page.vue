<template>
  <div class="min-h-screen flex items-center justify-center bg-background py-12 px-4 sm:px-6 lg:px-8">
    <div class="w-full max-w-md space-y-8">
      <div class="text-center">
        <h1 class="text-4xl font-bold text-foreground mb-2">Welcome!</h1>
        <p class="text-muted-foreground">Let's set up your organization</p>
      </div>
      
      <OrganizationForm @submit="handleOrganizationSubmit" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router'
import { useAuth } from '@/composables/useAuth'
import OrganizationForm from './components/OrganizationForm.vue'
import { organizationService } from '@/services/organizationService'

const router = useRouter()
const { user } = useAuth()

const handleOrganizationSubmit = async (form: { 
  name: string; 
  domain: string; 
  description: string; 
  address: string; 
  phone: string; 
  email: string 
}) => {
  try {
    const result = await organizationService.createOrganization({
      userId: user.value?.id || '',
      ...form
    })
    
    if (result.success) {
      // Organization created successfully, redirect to dashboard
      router.push('/dashboard')
    }
  } catch (error) {
    console.error('Error creating organization:', error)
  }
}
</script>
