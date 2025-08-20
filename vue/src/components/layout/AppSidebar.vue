<script setup lang="ts">
import { 
  LayoutDashboard, 
  CreditCard, 
  Package, 
  BarChart3, 
  Users, 
  Settings,
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
import OrganizationSelector from '@/components/navigation/OrganizationSelector.vue'


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

    <!-- Organization Selector -->
    <div class="border-b border-border p-4">
      <OrganizationSelector />
    </div>

    <SidebarContent class="overflow-hidden">

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
