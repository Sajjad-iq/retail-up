<template>
  <div class="flex h-screen bg-background">
    <!-- App Sidebar -->
    <AppSidebar />

    <!-- Main content area -->
    <div class="flex-1 flex flex-col">
      <!-- Top Navbar with Sidebar Trigger -->
      <div class="bg-card shadow-sm border-b border-border">
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
      <main class="flex-1 overflow-y-auto bg-muted/50">
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
