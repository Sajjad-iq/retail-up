<script setup lang="ts">
import { ref } from 'vue'
import { useAuthStore } from '@/stores/auth'
import MainLayout from '@/components/layout/MainLayout.vue'
import AuthPage from '@/components/auth/AuthPage.vue'
import OrganizationPage from '@/components/organization/OrganizationPage.vue'
import DashboardPage from '@/components/dashboard/DashboardPage.vue'
import ToastContainer from '@/components/ui/toast/ToastContainer.vue'

const authStore = useAuthStore()
const { user, organization, isAuthenticated } = authStore


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
