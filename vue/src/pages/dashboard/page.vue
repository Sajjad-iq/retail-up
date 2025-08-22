<template>
  <div class="p-6">
    <div class="mb-8 flex items-center justify-between">
      <div>
        <h1 class="text-3xl font-bold text-foreground">Dashboard</h1>
        <p class="mt-2 text-muted-foreground">Welcome back, {{ user?.name }}!</p>
      </div>
      <ThemeToggle />
    </div>

    <!-- Stats Grid -->
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
      <div class="bg-card rounded-lg border shadow-sm p-6">
        <div class="flex items-center">
          <div class="p-2 bg-blue-100 dark:bg-blue-900/20 rounded-lg">
            <svg
              class="w-6 h-6 text-blue-600 dark:text-blue-400"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1"
              />
            </svg>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-muted-foreground">Today's Sales</p>
            <p class="text-2xl font-semibold text-foreground">$2,450</p>
          </div>
        </div>
      </div>

      <div class="bg-card rounded-lg border shadow-sm p-6">
        <div class="flex items-center">
          <div class="p-2 bg-green-100 dark:bg-green-900/20 rounded-lg">
            <svg
              class="w-6 h-6 text-green-600 dark:text-green-400"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4"
              />
            </svg>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-muted-foreground">Total Products</p>
            <p class="text-2xl font-semibold text-foreground">1,234</p>
          </div>
        </div>
      </div>

      <div class="bg-card rounded-lg border shadow-sm p-6">
        <div class="flex items-center">
          <div class="p-2 bg-yellow-100 dark:bg-yellow-900/20 rounded-lg">
            <svg
              class="w-6 h-6 text-yellow-600 dark:text-yellow-400"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197m13.5-9a2.5 2.5 0 11-5 0 2.5 2.5 0 015 0z"
              />
            </svg>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-muted-foreground">Active Users</p>
            <p class="text-2xl font-semibold text-foreground">89</p>
          </div>
        </div>
      </div>

      <div class="bg-card rounded-lg border shadow-sm p-6">
        <div class="flex items-center">
          <div class="p-2 bg-purple-100 dark:bg-purple-900/20 rounded-lg">
            <svg
              class="w-6 h-6 text-purple-600 dark:text-purple-400"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
              />
            </svg>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-muted-foreground">Monthly Revenue</p>
            <p class="text-2xl font-semibold text-foreground">$45,678</p>
          </div>
        </div>
      </div>
    </div>

    <!-- Recent Activity -->
    <div class="bg-card rounded-lg border shadow-sm">
      <div class="px-6 py-4 border-b border-border">
        <h3 class="text-lg font-medium text-foreground">Recent Activity</h3>
      </div>
      <div class="p-6">
        <div class="space-y-4">
          <div class="flex items-center space-x-3">
            <div class="w-2 h-2 bg-blue-500 rounded-full"></div>
            <p class="text-sm text-muted-foreground">New sale completed - $125.00</p>
            <span class="text-xs text-muted-foreground/60">2 minutes ago</span>
          </div>
          <div class="flex items-center space-x-3">
            <div class="w-2 h-2 bg-green-500 rounded-full"></div>
            <p class="text-sm text-muted-foreground">Inventory updated - 50 items added</p>
            <span class="text-xs text-muted-foreground/60">15 minutes ago</span>
          </div>
          <div class="flex items-center space-x-3">
            <div class="w-2 h-2 bg-yellow-500 rounded-full"></div>
            <p class="text-sm text-muted-foreground">New user registered - John Doe</p>
            <span class="text-xs text-muted-foreground/60">1 hour ago</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useAuth } from "@/composables/useAuth";
import { ThemeToggle } from "@/components/ui/theme-toggle";

const { user } = useAuth();
</script>
