<script setup lang="ts">
import { computed } from 'vue'
import { 
  LayoutDashboard, 
  CreditCard, 
  Package, 
  BarChart3, 
  Users, 
  Settings,
  User,
  LogOut
} from 'lucide-vue-next'
import {
  Sidebar,
  SidebarContent,
  SidebarFooter,
  SidebarGroup,
  SidebarGroupContent,
  SidebarGroupLabel,
  SidebarHeader,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarSeparator
} from '@/components/ui/sidebar'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { useAuth } from '@/composables/useAuth'
import { useRouter } from 'vue-router'

const { user } = useAuth()
const router = useRouter()

// Menu items
const mainItems = [
  {
    title: "Dashboard",
    url: "/dashboard",
    icon: LayoutDashboard,
  },
  {
    title: "Point of Sale",
    url: "/pos",
    icon: CreditCard,
  },
  {
    title: "Inventory",
    url: "/inventory",
    icon: Package,
  },
  {
    title: "Reports",
    url: "/reports",
    icon: BarChart3,
  },
]

const managementItems = [
  {
    title: "Users",
    url: "/users",
    icon: Users,
  },
  {
    title: "Settings",
    url: "/settings",
    icon: Settings,
  },
]

const userInitials = computed(() => {
  if (!user.value?.name) return 'U'
  return user.value.name
    .split(' ')
    .map(n => n[0])
    .join('')
    .toUpperCase()
    .slice(0, 2)
})

const handleLogout = async () => {
  // Handle logout logic here
  console.log('Logout clicked')
}
</script>

<template>
  <Sidebar class="border-r border-border" data-sidebar>
    <SidebarHeader class="border-b border-border p-4">
      <div class="flex items-center space-x-2">
        <div class="w-8 h-8 bg-primary text-primary-foreground rounded-lg flex items-center justify-center">
          <span class="font-bold text-sm">RU</span>
        </div>
        <span class="font-semibold text-lg">Retail Up</span>
      </div>
    </SidebarHeader>

    <SidebarContent class="overflow-hidden">
      <!-- User Profile Section with Dropdown -->
      <SidebarGroup class="p-4 border-b border-border">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <button class="flex items-center space-x-3 w-full p-2 rounded-md hover:bg-accent hover:text-accent-foreground transition-colors">
              <div class="w-10 h-10 bg-primary rounded-full flex items-center justify-center">
                <span class="text-primary-foreground font-semibold text-sm">
                  {{ userInitials }}
                </span>
              </div>
              <div class="flex-1 min-w-0 text-left">
                <p class="text-sm font-medium text-foreground truncate">{{ user?.name || 'User' }}</p>
                <p class="text-xs text-muted-foreground truncate">{{ user?.email || 'user@example.com' }}</p>
              </div>
            </button>
          </DropdownMenuTrigger>
          <DropdownMenuContent class="w-56" align="start" side="right">
            <DropdownMenuLabel>
              <div class="flex flex-col space-y-1">
                <p class="text-sm font-medium leading-none">{{ user?.name || 'User' }}</p>
                <p class="text-xs leading-none text-muted-foreground">{{ user?.email || 'user@example.com' }}</p>
              </div>
            </DropdownMenuLabel>
            <DropdownMenuSeparator />
            <DropdownMenuItem @click="router.push('/profile')">
              <User class="mr-2 h-4 w-4" />
              <span>Profile</span>
            </DropdownMenuItem>
            <DropdownMenuItem @click="router.push('/settings')">
              <Settings class="mr-2 h-4 w-4" />
              <span>Settings</span>
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem @click="handleLogout" class="text-destructive">
              <LogOut class="mr-2 h-4 w-4" />
              <span>Log out</span>
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      </SidebarGroup>

      <!-- Main Navigation -->
      <SidebarGroup>
        <SidebarGroupLabel>Main</SidebarGroupLabel>
        <SidebarGroupContent>
          <SidebarMenu>
            <SidebarMenuItem v-for="item in mainItems" :key="item.title">
              <SidebarMenuButton asChild>
                <router-link :to="item.url" class="flex items-center space-x-3">
                  <component :is="item.icon" class="h-5 w-5" />
                  <span>{{ item.title }}</span>
                </router-link>
              </SidebarMenuButton>
            </SidebarMenuItem>
          </SidebarMenu>
        </SidebarGroupContent>
      </SidebarGroup>

      <SidebarSeparator />

      <!-- Management Navigation -->
      <SidebarGroup>
        <SidebarGroupLabel>Management</SidebarGroupLabel>
        <SidebarGroupContent>
          <SidebarMenu>
            <SidebarMenuItem v-for="item in managementItems" :key="item.title">
              <SidebarMenuButton asChild>
                <router-link :to="item.url" class="flex items-center space-x-3">
                  <component :is="item.icon" class="h-5 w-5" />
                  <span>{{ item.title }}</span>
                </router-link>
              </SidebarMenuButton>
            </SidebarMenuItem>
          </SidebarMenu>
        </SidebarGroupContent>
      </SidebarGroup>
    </SidebarContent>

    <SidebarFooter class="border-t border-border p-4">
      <div class="text-xs text-muted-foreground text-center">
        Retail Up v1.0.0
      </div>
    </SidebarFooter>
  </Sidebar>
</template>
