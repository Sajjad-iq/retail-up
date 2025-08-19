<template>
  <div class="flex h-screen bg-background w-full">
    <!-- App Sidebar -->
    <AppSidebar />

    <!-- Main content area -->
    <div class="flex-1 flex flex-col w-full">
      <!-- Mobile Header (only visible on mobile) -->
      <div class="lg:hidden bg-card shadow-sm border-b border-border">
        <div class="flex items-center justify-between px-4 py-3">
          <div class="flex items-center space-x-2">
            <SidebarTrigger class="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent focus:outline-none focus:ring-2 focus:ring-ring" />
            <div class="w-8 h-8 bg-primary text-primary-foreground rounded-lg flex items-center justify-center">
              <span class="font-bold text-sm">RU</span>
            </div>
            <span class="font-semibold text-lg text-foreground">Retail Up</span>
          </div>
        </div>
      </div>

      <!-- Desktop Navbar with Sidebar Trigger -->
      <div class="hidden lg:block bg-card shadow-sm border-b border-border">
        <div class="flex items-center justify-between px-4 py-3">
          <!-- Left side with toggle button -->
          <div class="flex items-center space-x-4">
            <SidebarTrigger class="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent focus:outline-none focus:ring-2 focus:ring-ring" />
            <span class="text-sm text-muted-foreground">Toggle Sidebar</span>
          </div>
          
          <!-- Right side with Navbar content -->
          <div class="flex-1">
            <Navbar />
          </div>
        </div>
      </div>

      <!-- Page content -->
      <main class="flex-1 overflow-y-auto bg-muted/50 p-4 w-full">
        <slot />
      </main>
    </div>
  </div>
</template>

<script setup lang="ts">
import AppSidebar from './AppSidebar.vue'
import Navbar from './Navbar.vue'
import { SidebarTrigger } from '@/components/ui/sidebar'
</script>
