<template>
  <nav class="flex items-center justify-between px-4 py-2">
    <!-- Left side - Organization Selector -->
    <div class="flex items-center">
      <OrganizationSelector />
    </div>

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

      <!-- User Menu -->
      <DropdownMenu>
        <DropdownMenuTrigger as-child>
          <Button variant="ghost" size="sm" class="h-8 w-8 p-0 rounded-full">
            <Avatar class="h-8 w-8">
              <AvatarFallback>{{ user?.name?.charAt(0).toUpperCase() }}</AvatarFallback>
            </Avatar>
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end" class="w-56">
          <DropdownMenuLabel>My Account</DropdownMenuLabel>
          <DropdownMenuSeparator />
          
          <!-- User Info -->
          <div class="px-2 py-1.5">
            <p class="text-sm font-medium">{{ user?.name }}</p>
            <p class="text-xs text-gray-500">{{ user?.email }}</p>
          </div>
          
          <DropdownMenuSeparator />
          
          <!-- Account Actions -->
          <DropdownMenuItem @click="goToProfile">
            <UserIcon class="mr-2 h-4 w-4" />
            Profile
          </DropdownMenuItem>
          
          <DropdownMenuItem @click="goToSettings">
            <Cog6ToothIcon class="mr-2 h-4 w-4" />
            Settings
          </DropdownMenuItem>
          
          <DropdownMenuSeparator />
          
          <!-- Logout -->
          <DropdownMenuItem @click="handleLogout" class="text-red-600">
            <ArrowRightOnRectangleIcon class="mr-2 h-4 w-4" />
            Logout
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </div>
  </nav>
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router'
import { useAuth } from '@/composables/useAuth'
import OrganizationSelector from '@/components/navigation/OrganizationSelector.vue'
import { Button } from '@/components/ui/button'
import { Avatar, AvatarFallback, AvatarImage } from '@/components/ui/avatar'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { Bell, MessageCircle } from 'lucide-vue-next'
import {
  UserIcon,
  Cog6ToothIcon,
  ArrowRightOnRectangleIcon,
} from '@heroicons/vue/24/outline'

const router = useRouter()
const { user, logout } = useAuth()

const goToProfile = () => {
  router.push('/profile')
}

const goToSettings = () => {
  router.push('/settings')
}

const handleLogout = async () => {
  await logout()
  router.push('/auth')
}
</script>
