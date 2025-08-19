<script setup lang="ts">
import MainLayout from '@/components/layout/MainLayout.vue'
import { Toaster } from '@/components/ui/sonner'
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
