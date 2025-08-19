<template>
  <nav class="bg-card shadow-sm border-b border-border">
    <div class="px-4 sm:px-6 lg:px-8">
      <div class="flex justify-between items-center h-16">
        <!-- Left side -->
        <div class="flex items-center space-x-4">
          <!-- Search Bar -->
          <div class="relative">
            <div class="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
              <svg class="h-5 w-5 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
              </svg>
            </div>
            <input
              type="text"
              placeholder="Search..."
              class="block w-64 pl-10 pr-3 py-2 border border-input rounded-md leading-5 bg-background placeholder-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:border-ring text-sm"
            />
          </div>
        </div>

        <!-- Right side -->
        <div class="flex items-center space-x-4">
          <!-- Notifications -->
          <button class="p-2 text-muted-foreground hover:text-foreground hover:bg-accent rounded-md relative">
            <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 17h5l-5 5v-5zM4.83 14.83a4 4 0 015.66-5.66l1.66 1.66m0 0l1.66-1.66a4 4 0 015.66 5.66l-1.66 1.66m0 0l-1.66 1.66a4 4 0 01-5.66-5.66l1.66-1.66z" />
            </svg>
            <!-- Notification badge -->
            <span class="absolute top-1 right-1 block h-2 w-2 rounded-full bg-destructive"></span>
          </button>

          <!-- Messages -->
          <button class="p-2 text-muted-foreground hover:text-foreground hover:bg-accent rounded-md relative">
            <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
            </svg>
            <!-- Message badge -->
            <span class="absolute top-1 right-1 block h-2 w-2 rounded-full bg-primary"></span>
          </button>

          <!-- Organization Selector -->
          <div class="relative">
            <button
              @click="toggleOrgDropdown"
              class="flex items-center space-x-2 px-3 py-2 text-sm font-medium text-foreground bg-background border border-input rounded-md hover:bg-accent hover:text-accent-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
            >
              <div class="w-6 h-6 bg-primary rounded flex items-center justify-center">
                <span class="text-primary-foreground text-xs font-bold">{{ organizationInitials }}</span>
              </div>
              <span class="hidden md:block">{{ organization?.name || 'Select Org' }}</span>
              <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
              </svg>
            </button>

            <!-- Organization Dropdown -->
            <div
              v-if="showOrgDropdown"
              class="absolute right-0 mt-2 w-56 bg-popover rounded-md shadow-lg ring-1 ring-border z-50"
            >
              <div class="py-1">
                <div class="px-4 py-2 text-sm text-popover-foreground border-b border-border">
                  <p class="font-medium">Current Organization</p>
                  <p class="text-muted-foreground">{{ organization?.name || 'None selected' }}</p>
                </div>
                <button
                  @click="createNewOrg"
                  class="block w-full text-left px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
                >
                  <div class="flex items-center space-x-2">
                    <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
                    </svg>
                    <span>Create New Organization</span>
                  </div>
                </button>
                <button
                  @click="manageOrgs"
                  class="block w-full text-left px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
                >
                  <div class="flex items-center space-x-2">
                    <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                    </svg>
                    <span>Manage Organizations</span>
                  </div>
                </button>
              </div>
            </div>
          </div>

          <!-- User Menu -->
          <div class="relative">
            <button
              @click="toggleUserDropdown"
              class="flex items-center space-x-3 p-2 text-sm font-medium text-muted-foreground hover:text-foreground hover:bg-accent rounded-md focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
            >
              <div class="w-8 h-8 bg-primary rounded-full flex items-center justify-center">
                <span class="text-primary-foreground text-sm font-semibold">{{ userInitials }}</span>
              </div>
              <div class="hidden md:block text-left">
                <p class="text-sm font-medium text-foreground">{{ user?.name }}</p>
                <p class="text-xs text-muted-foreground">{{ user?.email }}</p>
              </div>
              <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
              </svg>
            </button>

            <!-- User Dropdown -->
            <div
              v-if="showUserDropdown"
              class="absolute right-0 mt-2 w-48 bg-popover rounded-md shadow-lg ring-1 ring-border z-50"
            >
              <div class="py-1">
                <router-link
                  to="/profile"
                  class="block px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
                >
                  <div class="flex items-center space-x-2">
                    <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                    </svg>
                    <span>Your Profile</span>
                  </div>
                </router-link>
                <router-link
                  to="/settings"
                  class="block px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
                >
                  <div class="flex items-center space-x-2">
                    <svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                    </svg>
                    <span>Settings</span>
                  </div>
                </router-link>
                <div class="border-t border-border"></div>
                <button
                  @click="handleLogout"
                  class="block w-full text-left px-4 py-2 text-sm text-destructive hover:bg-destructive/10"
                >
                  <div class="flex items-center space-x-2">
                    <svg class="h-4 w-4 text-destructive" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" />
                    </svg>
                    <span>Sign out</span>
                  </div>
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Click outside to close dropdowns -->
    <div
      v-if="showOrgDropdown || showUserDropdown"
      @click="closeDropdowns"
      class="fixed inset-0 z-40"
    ></div>
  </nav>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { useRouter } from 'vue-router'
import { useAuth } from '@/composables/useAuth'

const router = useRouter()
const { user, organization, logout } = useAuth()

const showOrgDropdown = ref(false)
const showUserDropdown = ref(false)

const toggleOrgDropdown = () => {
  showOrgDropdown.value = !showOrgDropdown.value
  showUserDropdown.value = false
}

const toggleUserDropdown = () => {
  showUserDropdown.value = !showUserDropdown.value
  showOrgDropdown.value = false
}

const closeDropdowns = () => {
  showOrgDropdown.value = false
  showUserDropdown.value = false
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

const organizationInitials = computed(() => {
  if (!organization.value?.name) return 'O'
  return organization.value.name
    .split(' ')
    .map(n => n[0])
    .join('')
    .toUpperCase()
    .slice(0, 2)
})

const createNewOrg = () => {
  closeDropdowns()
  router.push('/organization/create')
}

const manageOrgs = () => {
  closeDropdowns()
  router.push('/organization/manage')
}

const handleLogout = async () => {
  closeDropdowns()
  await logout()
  router.push('/auth')
}

// Close dropdowns on escape key
const handleEscape = (event: KeyboardEvent) => {
  if (event.key === 'Escape') {
    closeDropdowns()
  }
}

onMounted(() => {
  document.addEventListener('keydown', handleEscape)
})

onUnmounted(() => {
  document.removeEventListener('keydown', handleEscape)
})
</script>
