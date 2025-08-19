<template>
  <div class="min-h-screen bg-gray-50">
    <!-- Navigation Header -->
    <header v-if="isAuthenticated" class="bg-white shadow-sm border-b">
      <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div class="flex justify-between items-center h-16">
          <div class="flex items-center">
            <h1 class="text-xl font-semibold text-gray-900">Retail Up</h1>
          </div>
          
          <div class="flex items-center space-x-4">
            <span class="text-sm text-gray-700">{{ user?.name }}</span>
            <Button variant="outline" @click="handleLogout">Logout</Button>
          </div>
        </div>
      </div>
    </header>

    <!-- Main Content -->
    <main>
      <slot />
    </main>
  </div>
</template>

<script setup lang="ts">
import { onMounted } from 'vue'
import { Button } from '@/components/ui/button'
import { useAuth } from '@/composables/useAuth'


const { user, isAuthenticated, logout, initialize } = useAuth()
const handleLogout = () => {
  logout()
}

onMounted(() => {
  initialize()
})
</script>
