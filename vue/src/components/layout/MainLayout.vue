<template>
  <div class="min-h-screen bg-gray-50">
    <!-- Navigation Header -->
    <AppNavigation v-if="isAuthenticated" />

    <!-- Main Content -->
    <main>
      <slot />
    </main>
  </div>
</template>

<script setup lang="ts">
import { onMounted } from 'vue'
import { useAuth } from '@/composables/useAuth'
import AppNavigation from '@/components/navigation/AppNavigation.vue'

const { isAuthenticated, initialize } = useAuth()

onMounted(() => {
  initialize()
})
</script>
