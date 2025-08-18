<template>
  <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
    <div class="mb-8">
      <h1 class="text-3xl font-bold text-gray-900">Dashboard</h1>
      <p class="text-gray-600">Welcome to your retail management dashboard</p>
    </div>
    
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      <!-- Organization Info Card -->
      <Card>
        <CardHeader>
          <CardTitle>Organization</CardTitle>
          <CardDescription>Your organization details</CardDescription>
        </CardHeader>
        <CardContent>
          <div class="space-y-2">
            <div>
              <span class="font-medium">Name:</span>
              <span class="ml-2">{{ organization?.name }}</span>
            </div>
            <div>
              <span class="font-medium">Domain:</span>
              <span class="ml-2">{{ organization?.domain }}</span>
            </div>
            <div>
              <span class="font-medium">Description:</span>
              <span class="ml-2">{{ organization?.description || 'No description' }}</span>
            </div>
            <div>
              <span class="font-medium">Address:</span>
              <span class="ml-2">{{ organization?.address || 'No address' }}</span>
            </div>
            <div>
              <span class="font-medium">Phone:</span>
              <span class="ml-2">{{ organization?.phone }}</span>
            </div>
            <div>
              <span class="font-medium">Status:</span>
              <span class="ml-2">
                <span :class="getStatusClass(organization?.status)">
                  {{ organization?.status }}
                </span>
              </span>
            </div>
            <div v-if="organization?.createdBy">
              <span class="font-medium">Created By:</span>
              <span class="ml-2">{{ organization.createdBy }}</span>
            </div>
          </div>
        </CardContent>
      </Card>
      
      <!-- User Info Card -->
      <Card>
        <CardHeader>
          <CardTitle>User</CardTitle>
          <CardDescription>Your account information</CardDescription>
        </CardHeader>
        <CardContent>
          <div class="space-y-2">
            <div>
              <span class="font-medium">Name:</span>
              <span class="ml-2">{{ user?.name }}</span>
            </div>
            <div>
              <span class="font-medium">Email:</span>
              <span class="ml-2">{{ user?.email }}</span>
            </div>
            <div>
              <span class="font-medium">Phone:</span>
              <span class="ml-2">{{ user?.phone }}</span>
            </div>
            <div>
              <span class="font-medium">Status:</span>
              <span class="ml-2">
                <span :class="getUserStatusClass(user?.status)">
                  {{ user?.status }}
                </span>
              </span>
            </div>
            <div>
              <span class="font-medium">Account Type:</span>
              <span class="ml-2">
                <span :class="getAccountTypeClass(user?.accountType)">
                  {{ user?.accountType }}
                </span>
              </span>
            </div>
            <div v-if="user?.createdAt">
              <span class="font-medium">Created:</span>
              <span class="ml-2">{{ formatDate(user.createdAt) }}</span>
            </div>
            <div v-if="user?.updatedAt">
              <span class="font-medium">Last Updated:</span>
              <span class="ml-2">{{ formatDate(user.updatedAt) }}</span>
            </div>
          </div>
        </CardContent>
      </Card>
      
      <!-- Quick Actions Card -->
      <Card>
        <CardHeader>
          <CardTitle>Quick Actions</CardTitle>
          <CardDescription>Common tasks</CardDescription>
        </CardHeader>
        <CardContent>
          <div class="space-y-3">
            <Button class="w-full" variant="outline">
              Manage Products
            </Button>
            <Button class="w-full" variant="outline">
              View Sales
            </Button>
            <Button class="w-full" variant="outline">
              Manage Inventory
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import type { OrganizationStatus, UserStatus, AccountType } from '@/services'

interface User {
  id: string
  name: string
  phone: string
  email: string
  status: UserStatus
  accountType: AccountType
  createdAt?: string // nullable in backend
  updatedAt?: string // nullable in backend
  lastLoginAt?: string
}

interface Organization {
  id: string
  name: string
  domain: string
  description?: string
  address?: string
  phone: string
  status: OrganizationStatus
  createdAt: string
  updatedAt: string
  createdBy: string // User ID as string (from DTO), not User object
}

defineProps<{
  user: User | null
  organization: Organization | null
}>()

const getStatusClass = (status: OrganizationStatus | undefined) => {
  switch (status) {
    case 'ACTIVE':
      return 'px-2 py-1 text-xs font-medium bg-green-100 text-green-800 rounded-full'
    case 'PENDING':
      return 'px-2 py-1 text-xs font-medium bg-yellow-100 text-yellow-800 rounded-full'
    case 'DISABLED':
      return 'px-2 py-1 text-xs font-medium bg-red-100 text-red-800 rounded-full'
    case 'REJECTED':
      return 'px-2 py-1 text-xs font-medium bg-red-100 text-red-800 rounded-full'
    case 'SUSPENDED':
      return 'px-2 py-1 text-xs font-medium bg-orange-100 text-orange-800 rounded-full'
    default:
      return 'px-2 py-1 text-xs font-medium bg-gray-100 text-gray-800 rounded-full'
  }
}

const getUserStatusClass = (status: UserStatus | undefined) => {
  switch (status) {
    case 'ACTIVE':
      return 'px-2 py-1 text-xs font-medium bg-green-100 text-green-800 rounded-full'
    case 'PENDING':
      return 'px-2 py-1 text-xs font-medium bg-yellow-100 text-yellow-800 rounded-full'
    case 'INACTIVE':
      return 'px-2 py-1 text-xs font-medium bg-gray-100 text-gray-800 rounded-full'
    case 'LOCKED':
      return 'px-2 py-1 text-xs font-medium bg-red-100 text-red-800 rounded-full'
    default:
      return 'px-2 py-1 text-xs font-medium bg-gray-100 text-gray-800 rounded-full'
  }
}

const getAccountTypeClass = (accountType: AccountType | undefined) => {
  switch (accountType) {
    case 'USER':
      return 'px-2 py-1 text-xs font-medium bg-blue-100 text-blue-800 rounded-full'
    case 'EMPLOYEE':
      return 'px-2 py-1 text-xs font-medium bg-purple-100 text-purple-800 rounded-full'
    default:
      return 'px-2 py-1 text-xs font-medium bg-gray-100 text-gray-800 rounded-full'
  }
}

const formatDate = (dateString: string) => {
  try {
    return new Date(dateString).toLocaleDateString()
  } catch {
    return dateString
  }
}
</script>
