<script setup lang="ts">
import { computed, onMounted } from 'vue'
import { SidebarProvider } from '@/components/ui/sidebar'
import MainLayout from '@/components/layout/MainLayout.vue'
import { Toaster } from '@/components/ui/sonner'
import { useAuth } from '@/composables/useAuth'

const { isAuthenticated ,initialize} = useAuth()

// Show main layout for authenticated users (including organization setup)
// but not for auth routes
const showMainLayout = computed(() => {
  return isAuthenticated.value
})

onMounted(() => {
  initialize()
})
</script>

<template>
  <SidebarProvider>
    <!-- Show main layout for authenticated users (including organization setup) -->
    <MainLayout v-if="showMainLayout">
      <router-view />
    </MainLayout>
    
    <!-- Show auth routes without main layout -->
    <router-view v-else />
    
    <!-- Toast notifications -->
    <Toaster />
  </SidebarProvider>
</template>

<style>
@import './assets/main.css';
</style>
