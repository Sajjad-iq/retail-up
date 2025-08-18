<script setup lang="ts">
import { useAuthStore } from '@/stores/auth'
import MainLayout from '@/components/layout/MainLayout.vue'
import AuthPage from '@/pages/auth/page.vue'
import OrganizationPage from '@/components/organization/OrganizationPage.vue'
import DashboardPage from '@/pages/dashboard/page.vue'
import ToastContainer from '@/components/ui/toast/ToastContainer.vue'
import { onMounted } from 'vue'

const authStore = useAuthStore()
const { user, organization, isAuthenticated } = authStore

onMounted(() => {
  authStore.initializeAuth()
})


</script>

<template>
  <MainLayout>
    <!-- Show auth page if not authenticated -->
    <AuthPage
      v-if="!isAuthenticated"
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
  <ToastContainer ref="toastContainer" />
</template>

<style>
@import './assets/main.css';
</style>
