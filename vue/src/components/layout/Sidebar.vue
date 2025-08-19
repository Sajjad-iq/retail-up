<template>
  <div class="flex h-screen bg-background">
    <!-- Sidebar -->
    <div 
      class="fixed inset-y-0 left-0 z-50 w-64 bg-card shadow-lg transform transition-transform duration-300 ease-in-out border-r border-border"
      :class="{ '-translate-x-full': !isOpen, 'translate-x-0': isOpen }"
    >
      <!-- Sidebar Header -->
      <div class="flex items-center justify-between h-16 px-6 bg-primary text-primary-foreground">
        <div class="flex items-center space-x-2">
          <div class="w-8 h-8 bg-primary-foreground text-primary rounded-lg flex items-center justify-center">
            <span class="font-bold text-sm">RU</span>
          </div>
          <span class="font-semibold text-lg">Retail Up</span>
        </div>
        <button
          @click="toggleSidebar"
          class="p-1 rounded-md hover:bg-primary-foreground/20 focus:outline-none focus:ring-2 focus:ring-primary-foreground/50"
        >
          <svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
            <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd" />
          </svg>
        </button>
      </div>

      <!-- User Profile Section -->
      <div class="px-6 py-4 border-b border-border">
        <div class="flex items-center space-x-3">
          <div class="w-10 h-10 bg-primary rounded-full flex items-center justify-center">
            <span class="text-primary-foreground font-semibold text-sm">
              {{ userInitials }}
            </span>
          </div>
          <div class="flex-1 min-w-0">
            <p class="text-sm font-medium text-foreground truncate">{{ user?.name || 'User' }}</p>
            <p class="text-xs text-muted-foreground truncate">{{ user?.email || 'user@example.com' }}</p>
          </div>
        </div>
      </div>

      <!-- Navigation Menu -->
      <nav class="flex-1 px-4 py-6 space-y-2">
        <div class="space-y-1">
          <h3 class="px-3 text-xs font-semibold text-muted-foreground uppercase tracking-wider">
            Main
          </h3>
          
          <!-- Dashboard -->
          <router-link
            to="/dashboard"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/dashboard'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2H5a2 2 0 00-2-2z" />
            </svg>
            Dashboard
          </router-link>

          <!-- POS -->
          <router-link
            to="/pos"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/pos'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1" />
            </svg>
            Point of Sale
          </router-link>

          <!-- Inventory -->
          <router-link
            to="/inventory"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/inventory'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
            </svg>
            Inventory
          </router-link>

          <!-- Reports -->
          <router-link
            to="/reports"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/reports'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
            </svg>
            Reports
          </router-link>
        </div>

        <div class="space-y-1">
          <h3 class="px-3 text-xs font-semibold text-muted-foreground uppercase tracking-wider">
            Management
          </h3>
          
          <!-- Users -->
          <router-link
            to="/users"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/users'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197m13.5-9a2.5 2.5 0 11-5 0 2.5 2.5 0 015 0z" />
            </svg>
            Users
          </router-link>

          <!-- Settings -->
          <router-link
            to="/settings"
            class="group flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors"
            :class="[
              $route.path === '/settings'
                ? 'bg-primary/10 text-primary border-r-2 border-primary'
                : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground'
            ]"
          >
            <svg class="mr-3 h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
            </svg>
            Settings
          </router-link>
        </div>
      </nav>
    </div>

    <!-- Mobile overlay -->
    <div
      v-if="isOpen"
      @click="closeSidebar"
      class="fixed inset-0 z-40 bg-background/80 backdrop-blur-sm lg:hidden"
    ></div>

    <!-- Main content area -->
    <div class="flex-1 flex flex-col lg:ml-64">
      <!-- Mobile header -->
      <div class="lg:hidden bg-card shadow-sm border-b border-border">
        <div class="flex items-center justify-between px-4 py-3">
          <button
            @click="toggleSidebar"
            class="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent focus:outline-none focus:ring-2 focus:ring-inset focus:ring-ring"
          >
            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16" />
            </svg>
          </button>
          <div class="flex items-center space-x-2">
            <div class="w-8 h-8 bg-primary text-primary-foreground rounded-lg flex items-center justify-center">
              <span class="font-bold text-sm">RU</span>
            </div>
            <span class="font-semibold text-lg text-foreground">Retail Up</span>
          </div>
        </div>
      </div>

      <!-- Top Navbar -->
      <Navbar />

      <!-- Page content -->
      <main class="flex-1 overflow-y-auto bg-muted/50">
        <slot />
      </main>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue'
import { useAuth } from '@/composables/useAuth'
import Navbar from './Navbar.vue'

const { user } = useAuth()

const isOpen = ref(false)

const toggleSidebar = () => {
  isOpen.value = !isOpen.value
}

const closeSidebar = () => {
  isOpen.value = false
}

const userInitials = computed(() => {
  if (!user.value?.name) return 'U'
  return user.value.name
    .split(' ')
    .map(n => n[0])
    .join('')
    .toUpperCase()
    .slice(0, 2)
})
</script>
