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
const toastContainer = ref<InstanceType<typeof ToastContainer>>()

const showToast = (type: 'success' | 'error' | 'info', title: string, message: string) => {
  toastContainer.value?.addToast({ type, title, message })
}

const handleLoginSuccess = () => {
  console.log('Login successful')
  showToast('success', 'Success', 'Login successful!')
}

const handleRegisterSuccess = () => {
  console.log('Registration successful')
  showToast('success', 'Success', 'Account created successfully!')
}

const handleOrganizationCreated = () => {
  console.log('Organization created successfully')
  showToast('success', 'Success', 'Organization created successfully!')
}
</script>

<template>
  <MainLayout>
    <!-- Show auth page if not authenticated -->
    <AuthPage
      v-if="!isAuthenticated"
      @login-success="handleLoginSuccess"
      @register-success="handleRegisterSuccess"
    />
    
    <!-- Show organization creation if authenticated but no organization -->
    <OrganizationPage
      v-else-if="!organization"
      @organization-created="handleOrganizationCreated"
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
