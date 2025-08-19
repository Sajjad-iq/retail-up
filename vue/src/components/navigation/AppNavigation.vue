<template>
  <nav class="bg-white shadow-sm border-b">
    <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
      <div class="flex justify-between h-16">
        <!-- Logo/Brand -->
        <div class="flex items-center">
          <router-link to="/dashboard" class="flex-shrink-0">
            <h1 class="text-xl font-bold text-gray-900">Retail UP</h1>
          </router-link>
        </div>

        <!-- Navigation Links -->
        <div class="hidden md:flex items-center space-x-8">
          <router-link
            v-for="route in availableRoutes"
            :key="route.name"
            :to="{ name: route.name }"
            class="text-gray-700 hover:text-gray-900 px-3 py-2 rounded-md text-sm font-medium transition-colors"
            :class="{ 'bg-gray-100 text-gray-900': $route.name === route.name }"
          >
            {{ route.meta?.title }}
          </router-link>
        </div>

        <!-- User Menu -->
        <div class="flex items-center space-x-4">
          <div class="text-sm text-gray-700">
            {{ user?.name }}
          </div>
          <button
            @click="handleLogout"
            class="text-gray-700 hover:text-gray-900 px-3 py-2 rounded-md text-sm font-medium transition-colors"
          >
            Logout
          </button>
        </div>
      </div>
    </div>
  </nav>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useRouter } from 'vue-router'
import { useAuthStore } from '@/stores/auth'
import { useAuth } from '@/composables/useAuth'
import { routes } from '@/router'

const router = useRouter()
const authStore = useAuthStore()
const { logout } = useAuth()

const user = computed(() => authStore.user)

// Filter routes based on authentication only
const availableRoutes = computed(() => {
  if (!authStore.isAuthenticated) return []
  
  return routes.filter(route => {
    // Skip auth, organization setup, and not found routes
    if (['Auth', 'Organization', 'NotFound'].includes(route.name as string)) {
      return false
    }

    // Show all authenticated routes
    return route.meta?.requiresAuth
  })
})

const handleLogout = async () => {
  await logout()
  router.push({ name: 'Auth' })
}
</script>
