<template>
  <nav class="flex items-center justify-end space-x-4 px-4 py-2">

    <!-- Right side actions -->
    <div class="flex items-center space-x-2">
      <!-- Notifications -->
      <button class="p-2 text-muted-foreground hover:text-foreground hover:bg-accent rounded-md relative">
        <Bell class="h-5 w-5" />
        <!-- Notification badge -->
        <span class="absolute top-1 right-1 block h-2 w-2 rounded-full bg-destructive"></span>
      </button>

      <!-- Messages -->
      <button class="p-2 text-muted-foreground hover:text-foreground hover:bg-accent rounded-md relative">
        <MessageCircle class="h-5 w-5" />
        <!-- Message badge -->
        <span class="absolute top-1 right-1 block h-2 w-2 rounded-full bg-primary"></span>
      </button>

      </div>
  </nav>
</template>

<script setup lang="ts">
import { Bell, MessageCircle } from 'lucide-vue-next'
  </script>
