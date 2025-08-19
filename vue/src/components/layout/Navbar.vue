<template>
  <nav class="flex items-center justify-end space-x-4 px-4 py-2">
    <!-- Search Bar (hidden on mobile) -->
    <div class="hidden md:block relative">
      <div class="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
        <Search class="h-5 w-5 text-muted-foreground" />
      </div>
      <input
        type="text"
        placeholder="Search..."
        class="block w-64 pl-10 pr-3 py-2 border border-input rounded-md leading-5 bg-background placeholder-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:border-ring text-sm"
      />
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

      <!-- Organization Selector (hidden on small screens) -->
      <div class="hidden sm:block relative">
        <button
          @click="toggleOrgDropdown"
          class="flex items-center space-x-2 px-3 py-2 text-sm font-medium text-foreground bg-background border border-input rounded-md hover:bg-accent hover:text-accent-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
        >
          <div class="w-6 h-6 bg-primary rounded flex items-center justify-center">
            <span class="text-primary-foreground text-xs font-bold">{{ organizationInitials }}</span>
          </div>
          <span class="hidden md:block">{{ organization?.name || 'Select Org' }}</span>
          <ChevronDown class="h-4 w-4 text-muted-foreground" />
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
                <Plus class="h-4 w-4 text-muted-foreground" />
                <span>Create New Organization</span>
              </div>
            </button>
            <button
              @click="manageOrgs"
              class="block w-full text-left px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
            >
              <div class="flex items-center space-x-2">
                <Settings class="h-4 w-4 text-muted-foreground" />
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
          class="flex items-center space-x-2 p-2 text-sm font-medium text-muted-foreground hover:text-foreground hover:bg-accent rounded-md focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
        >
          <div class="w-8 h-8 bg-primary rounded-full flex items-center justify-center">
            <span class="text-primary-foreground text-sm font-semibold">{{ userInitials }}</span>
          </div>
          <div class="hidden md:block text-left">
            <p class="text-sm font-medium text-foreground">{{ user?.name }}</p>
            <p class="text-xs text-muted-foreground">{{ user?.email }}</p>
          </div>
          <ChevronDown class="h-4 w-4 text-muted-foreground" />
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
                <User class="h-4 w-4 text-muted-foreground" />
                <span>Your Profile</span>
              </div>
            </router-link>
            <router-link
              to="/settings"
              class="block px-4 py-2 text-sm text-popover-foreground hover:bg-accent hover:text-accent-foreground"
            >
              <div class="flex items-center space-x-2">
                <Settings class="h-4 w-4 text-muted-foreground" />
                <span>Settings</span>
              </div>
            </router-link>
            <div class="border-t border-border"></div>
            <button
              @click="handleLogout"
              class="block w-full text-left px-4 py-2 text-sm text-destructive hover:bg-destructive/10"
            >
              <div class="flex items-center space-x-2">
                <LogOut class="h-4 w-4 text-destructive" />
                <span>Sign out</span>
              </div>
            </button>
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
import { 
  Search, 
  Bell, 
  MessageCircle, 
  ChevronDown, 
  Plus, 
  Settings, 
  User, 
  LogOut 
} from 'lucide-vue-next'

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
