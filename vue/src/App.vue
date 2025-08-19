<script setup lang="ts">
import { useAuthStore } from '@/stores/auth'
import MainLayout from '@/components/layout/MainLayout.vue'
import { Toaster } from '@/components/ui/sonner'

const { isAuthenticated } = useAuthStore()
</script>

<template>
  <MainLayout>
    <!-- Router view will handle all routing -->
    <router-view />
  </MainLayout>
  <!-- Toast notifications -->
  <Toaster />
</template>

<style>
@import './assets/main.css';
</style>
