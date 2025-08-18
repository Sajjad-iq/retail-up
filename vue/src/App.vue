<script setup lang="ts">
import { useAuth } from '@/composables/useAuth'
import MainLayout from '@/components/layout/MainLayout.vue'
import AuthPage from '@/pages/auth/page.vue'
import OrganizationPage from '@/pages/organization/page.vue'
import DashboardPage from '@/pages/dashboard/page.vue'
import { Toaster } from '@/components/ui/sonner'

const { user, organization, isAuthenticated, isLoading, isInitialized } = useAuth()
</script>

<template>
  <MainLayout>
    <!-- Show loading state while initializing -->
    <div v-if="!isInitialized || isLoading" class="min-h-screen flex items-center justify-center">
      <div class="text-center">
        <div class="animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900 mx-auto mb-4"></div>
        <p class="text-gray-600">Initializing...</p>
      </div>
    </div>
    
    <!-- Show auth page if not authenticated -->
    <AuthPage
      v-else-if="!isAuthenticated"
    />
    
    <!-- Show organization creation if authenticated but no organization -->
    <OrganizationPage
      v-else-if="!organization"
    />
    
    <!-- Show main dashboard if authenticated and has organization -->
    <DashboardPage
      v-else
      :user="user"
      :organization="organization"
    />
  </MainLayout>
  
  <!-- Toast notifications -->
  <Toaster />
</template>

<style>
@import './assets/main.css';
</style>
