<template>
  <div class="min-h-screen bg-background">
    <Sidebar v-if="isAuthenticated" />
    <div v-else class="min-h-screen">
      <slot />
    </div>
  </div>
</template>

<script setup lang="ts">
import { useAuth } from '@/composables/useAuth'
import Sidebar from './Sidebar.vue'

const { isAuthenticated } = useAuth()
</script>
