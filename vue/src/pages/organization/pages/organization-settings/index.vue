<template>
  <div class="min-h-screen bg-gray-50 p-8">
    <div class="max-w-4xl mx-auto">

      <!-- Loading State -->
      <div v-if="isLoading" class="flex items-center justify-center py-12">
        <div class="text-center">
          <div class="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p class="mt-4 text-gray-600">Loading organization details...</p>
        </div>
      </div>

      <!-- Error State -->
      <div v-else-if="error" class="bg-red-50 border border-red-200 rounded-lg p-6">
        <div class="flex items-center gap-3">
          <ExclamationTriangleIcon class="h-6 w-6 text-red-600" />
          <div>
            <h3 class="text-lg font-medium text-red-800">Error Loading Organization</h3>
            <p class="text-red-700 mt-1">{{ error }}</p>
          </div>
        </div>
        <Button @click="loadOrganization" variant="outline" class="mt-4">
          Try Again
        </Button>
      </div>

      <!-- Organization Settings Form -->
      <div v-else-if="organization" class="space-y-8">
        <!-- Organization Info Card -->
        <Card class="p-6">
          <CardHeader>
            <CardTitle class="flex items-center gap-3">
              <BuildingOfficeIcon class="h-6 w-6 text-blue-600" />
              Organization Information
            </CardTitle>
            <CardDescription>
              Update your organization's basic information and contact details
            </CardDescription>
          </CardHeader>
          <CardContent>
            <form @submit="onSubmit" class="space-y-6">
              <!-- Name and Domain Row -->
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <FormField v-slot="{ componentField }" name="name">
                  <FormItem>
                    <FormLabel>Organization Name *</FormLabel>
                    <FormControl>
                      <Input
                        type="text"
                        placeholder="Enter organization name"
                        :disabled="isSubmitting"
                        v-bind="componentField"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                </FormField>

                <FormField v-slot="{ componentField }" name="domain">
                  <FormItem>
                    <FormLabel>Domain *</FormLabel>
                    <FormControl>
                      <Input
                        type="text"
                        placeholder="yourcompany.com"
                        :disabled="isSubmitting"
                        v-bind="componentField"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                </FormField>
              </div>

              <!-- Description -->
              <FormField v-slot="{ componentField }" name="description">
                <FormItem>
                  <FormLabel>Description</FormLabel>
                  <FormControl>
                    <Input
                      type="text"
                      placeholder="Brief description of your organization"
                      :disabled="isSubmitting"
                      v-bind="componentField"
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              </FormField>

              <!-- Address -->
              <FormField v-slot="{ componentField }" name="address">
                <FormItem>
                  <FormLabel>Address</FormLabel>
                  <FormControl>
                    <Input
                      type="text"
                      placeholder="Organization address"
                      :disabled="isSubmitting"
                      v-bind="componentField"
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              </FormField>

              <!-- Contact Information Row -->
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <FormField v-slot="{ componentField }" name="phone">
                  <FormItem>
                    <FormLabel>Phone Number *</FormLabel>
                    <FormControl>
                      <Input
                        type="tel"
                        placeholder="+1 (555) 123-4567"
                        :disabled="isSubmitting"
                        v-bind="componentField"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                </FormField>

                <FormField v-slot="{ componentField }" name="status">
                  <FormItem>
                    <FormLabel>Status</FormLabel>
                    <FormControl>
                      <Select v-bind="componentField" :disabled="isSubmitting">
                        <SelectTrigger>
                          <SelectValue placeholder="Select status" />
                        </SelectTrigger>
                        <SelectContent>
                          <SelectItem value="ACTIVE">Active</SelectItem>
                          <SelectItem value="DISABLED">Disabled</SelectItem>
                          <SelectItem value="PENDING">Pending</SelectItem>
                          <SelectItem value="REJECTED">Rejected</SelectItem>
                          <SelectItem value="SUSPENDED">Suspended</SelectItem>
                          <SelectItem value="DELETED">Deleted</SelectItem>
                        </SelectContent>
                      </Select>
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                </FormField>
              </div>

              <!-- Action Buttons -->
              <div class="flex items-center justify-end gap-4 pt-4 border-t">
                <Button
                  type="button"
                  variant="outline"
                  @click="resetForm"
                  :disabled="isSubmitting"
                >
                  Reset
                </Button>
                <Button
                  type="submit"
                  :disabled="isSubmitting"
                  class="min-w-[120px]"
                >
                  <span v-if="isSubmitting" class="flex items-center gap-2">
                    <div class="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
                    Saving...
                  </span>
                  <span v-else>Save Changes</span>
                </Button>
              </div>
            </form>
          </CardContent>
        </Card>

        <!-- Organization Stats Card -->
        <Card class="p-6">
          <CardHeader>
            <CardTitle class="flex items-center gap-3">
              <ChartBarIcon class="h-6 w-6 text-green-600" />
              Organization Statistics
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div class="text-center">
                <div class="text-2xl font-bold text-blue-600">{{ organization.status }}</div>
                <div class="text-sm text-gray-600">Current Status</div>
              </div>
              <div class="text-center">
                <div class="text-2xl font-bold text-green-600">
                  {{ formatDate(organization.createdAt) }}
                </div>
                <div class="text-sm text-gray-600">Created</div>
              </div>
              <div class="text-center">
                <div class="text-2xl font-bold text-purple-600">
                  {{ formatDate(organization.updatedAt) }}
                </div>
                <div class="text-sm text-gray-600">Last Updated</div>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <!-- No Organization Selected -->
      <div v-else class="text-center py-12">
        <BuildingOfficeIcon class="h-16 w-16 text-gray-400 mx-auto mb-4" />
        <h3 class="text-lg font-medium text-gray-900 mb-2">No Organization Selected</h3>
        <p class="text-gray-600 mb-6">Please select an organization to view and edit its settings.</p>
        <Button @click="goToOrganizationSelection" variant="outline">
          Select Organization
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import { useRouter } from 'vue-router'
import { useForm } from 'vee-validate'
import { toTypedSchema } from '@vee-validate/zod'
import * as z from 'zod'
import { useOrganization } from '@/composables/useOrganization'
import { organizationService } from '@/services/organizationService'
import { toast } from 'vue-sonner'
import type { OrganizationResponse, UpdateOrganizationRequest } from '@/services/organizationService'
import { OrganizationStatus } from '@/types/global'

// UI Components
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import {
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'

// Icons
import {
  BuildingOfficeIcon,
  ExclamationTriangleIcon,
  ChartBarIcon,
} from '@heroicons/vue/24/outline'
import { useAuth } from '@/composables/useAuth'

const router = useRouter()
const { selectedOrganization, getOrganizationById } = useOrganization()

// State
const organization = ref<OrganizationResponse | null>(null)
const isLoading = ref(false)
const isSubmitting = ref(false)
const error = ref<string | null>(null)
const{ user} = useAuth()

// Form validation schema
const formSchema = toTypedSchema(z.object({
  name: z.string().min(1, 'Organization name is required'),
  description: z.string().optional(),
  address: z.string().optional(),
  phone: z.string().min(1, 'Phone number is required'),
  domain: z.string().min(1, 'Domain is required'),
  status: z.nativeEnum(OrganizationStatus)
}))

// Form instance
const form = useForm({
  validationSchema: formSchema,
  initialValues: {
    name: '',
    description: '',
    address: '',
    phone: '',
    domain: '',
    status: OrganizationStatus.ACTIVE
  }
})

// Computed


// Methods
const loadOrganization = async () => {
  if (!selectedOrganization.value?.id) {
    error.value = 'No organization selected'
    return
  }

  isLoading.value = true
  error.value = null

  try {
    // Use the composable method instead of calling the service directly
    const orgData = await getOrganizationById(selectedOrganization.value.id)
    
    if (orgData) {
      organization.value = orgData
      populateForm(orgData)
    } else {
      error.value = 'Failed to load organization details'
    }
  } catch (err) {
    error.value = 'An error occurred while loading organization details'
  } finally {
    isLoading.value = false
  }
}

const populateForm = (org: OrganizationResponse) => {
  const formData = {
    name: org.name || '',
    description: org.description || '',
    address: org.address || '',
    phone: org.phone || '',
    domain: org.domain || '',
    status: org.status || OrganizationStatus.ACTIVE
  }
    form.setValues(formData)
  }

const resetForm = () => {
  if (organization.value) {
    populateForm(organization.value)
  }
}

const handleSubmit = async (values: any) => {
  if (!organization.value) return
  if (!user.value?.id) return

  isSubmitting.value = true

  try {
    const updateData: Omit<UpdateOrganizationRequest, 'userId'> = {
      name: values.name,
      description: values.description,
      address: values.address,
      phone: values.phone,
      domain: values.domain,
      status: values.status
    }

    const result = await organizationService.updateOrganization(
      organization.value.id,
      user.value.id, // Using selected org ID as userId for now
      updateData
    )

    if (result.success && result.data) {
      organization.value = result.data
      toast.success('Organization updated successfully')
      
      // Update the selected organization in the store
      // This would typically be handled by the composable
    } else {
      toast.error(result.error || 'Failed to update organization')
    }
  } catch (err) {
    toast.error('An error occurred while updating organization')
  } finally {
    isSubmitting.value = false
  }
}

const onSubmit = form.handleSubmit(async (values) => {
  await handleSubmit(values)
})

const goToOrganizationSelection = () => {
  router.push('/organization-selection')
}

const formatDate = (dateString: string): string => {
  if (!dateString) return 'N/A'
  try {
    return new Date(dateString).toLocaleDateString()
  } catch {
    return 'Invalid Date'
  }
}

// Watch for organization changes
watch(selectedOrganization, (newOrg) => {
  if (newOrg) {
    loadOrganization()
  } else {
    toast.info('No organization selected')
    organization.value = null
  }
}, { immediate: true })

// Load organization on mount
onMounted(() => {
  if (selectedOrganization.value) {
    loadOrganization()
  } else {
    toast.info('No organization selected on mount')
  }
})
</script>

