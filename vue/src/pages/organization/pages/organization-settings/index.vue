<template>
  <!-- Main container with responsive padding and max width -->
  <div class="min-h-screen bg-background p-8">
    <div class="max-w-4xl mx-auto">
      <!-- Loading State: Shows spinner while loading -->

      <!-- Error State: Displays error message with retry button -->
      <div
        v-if="error"
        class="bg-red-50 dark:bg-red-950/20 border border-red-200 dark:border-red-800 rounded-lg p-6"
      >
        <div class="flex items-center gap-3">
          <ExclamationTriangleIcon class="h-6 w-6 text-red-600 dark:text-red-400" />
          <div>
            <h3 class="text-lg font-medium text-red-800 dark:text-red-200">
              Error Loading Organization
            </h3>
            <p class="text-red-700 dark:text-red-300 mt-1">{{ error }}</p>
          </div>
        </div>
        <Button @click="loadOrganization" variant="outline" class="mt-4"> Try Again </Button>
      </div>

      <!-- Organization Settings Form: Main content when organization is loaded -->
      <div v-else-if="organization" class="space-y-8">
        <!-- Organization Info Card: Form for editing organization details -->
        <Card class="p-6">
          <CardHeader>
            <div class="flex items-center justify-between">
              <div class="flex items-center gap-3">
                <BuildingOfficeIcon class="h-6 w-6 text-blue-600 dark:text-blue-400" />
                <div>
                  <CardTitle>Organization Information</CardTitle>
                  <CardDescription>
                    Update your organization's basic information and contact details
                  </CardDescription>
                </div>
              </div>
              <div class="flex items-center gap-3">
                <ThemeToggle />
                <Button
                  variant="outline"
                  size="sm"
                  @click="refreshOrganization"
                  :disabled="isLoading"
                  class="flex items-center gap-2"
                >
                  <ArrowPathIcon class="h-4 w-4" />
                  Refresh
                </Button>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <!-- Form with validation using vee-validate -->
            <form @submit="onSubmit" class="space-y-6">
              <!-- Name and Domain Row: Two-column layout for name and domain -->
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

              <!-- Description: Single line input for organization description -->
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

              <!-- Address: Single line input for organization address -->
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

              <!-- Contact Information Row: Phone and status in two-column layout -->
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

                <!-- Status dropdown: Select component for organization status -->
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
                        </SelectContent>
                      </Select>
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                </FormField>
              </div>

              <!-- Action Buttons: Reset and Save buttons with loading states -->
              <div class="flex items-center justify-end gap-4 pt-4 border-t">
                <Button type="button" variant="outline" @click="resetForm" :disabled="isSubmitting">
                  Reset
                </Button>
                <Button type="submit" :disabled="isSubmitting" class="min-w-[120px]">
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

        <!-- Organization Stats Card: Display organization statistics -->
        <Card class="p-6">
          <CardHeader>
            <CardTitle class="flex items-center gap-3">
              <ChartBarIcon class="h-6 w-6 text-green-600" />
              Organization Statistics
            </CardTitle>
          </CardHeader>
          <CardContent>
            <!-- Three-column grid showing status, created date, and last updated date -->
            <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div class="text-center">
                <div class="text-2xl font-bold text-blue-600 dark:text-blue-400">
                  {{ organization.status }}
                </div>
                <div class="text-sm text-muted-foreground">Current Status</div>
              </div>
              <div class="text-center">
                <div class="text-2xl font-bold text-green-600 dark:text-green-400">
                  {{ formatDate(organization.createdAt) }}
                </div>
                <div class="text-sm text-muted-foreground">Created</div>
              </div>
              <div class="text-center">
                <div class="text-2xl font-bold text-purple-600 dark:text-purple-400">
                  {{ formatDate(organization.updatedAt) }}
                </div>
                <div class="text-sm text-muted-foreground">Last Updated</div>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <!-- No Organization Selected: Fallback state when no organization is available -->
      <div v-else-if="isAuthInitialized && !isAuthLoading" class="text-center py-12">
        <BuildingOfficeIcon class="h-16 w-16 text-muted-foreground mx-auto mb-4" />
        <h3 class="text-lg font-medium text-foreground mb-2">No Organization Selected</h3>
        <p class="text-muted-foreground mb-6">
          Please select an organization to view and edit its settings.
        </p>
        <Button @click="goToOrganizationSelection" variant="outline"> Select Organization </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Vue Composition API imports
import { ref, onMounted, watch } from "vue";
import { useRouter } from "vue-router";

// Form validation imports
import { useForm } from "vee-validate";
import { toTypedSchema } from "@vee-validate/zod";
import * as z from "zod";

// Custom composables and services
import { useOrganization } from "@/composables/useOrganization";
import { useAuth } from "@/composables/useAuth";
import { formatDate } from "@/lib/utils";

// Toast notification
import { toast } from "vue-sonner";

// Type imports
import type {
  OrganizationResponse,
  UpdateOrganizationRequest,
} from "@/services/organizationService";
import { OrganizationStatus } from "@/types/global";

// UI Components - Form elements
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { ThemeToggle } from "@/components/ui/theme-toggle";
// Reusable spinner component for loading states with customizable colors and messages
import { Spinner } from "@/components/ui/spinner";
import { FormControl, FormField, FormItem, FormLabel, FormMessage } from "@/components/ui/form";

// UI Components - Select dropdown
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

// Heroicons for visual elements
import {
  BuildingOfficeIcon,
  ExclamationTriangleIcon,
  ChartBarIcon,
  ArrowPathIcon,
} from "@heroicons/vue/24/outline";

// Router instance for navigation
const router = useRouter();

// Organization composable for managing organization state
const { selectedOrganization, getOrganizationById, updateOrganization } = useOrganization();

// Authentication composable for user information
const { user, isLoading: isAuthLoading, isInitialized: isAuthInitialized } = useAuth();

// ===== REACTIVE STATE =====
// Current organization data
const organization = ref<OrganizationResponse | null>(null);
// Loading state for initial data fetch (organization details)
const isLoading = ref(false);
// Loading state for form submission
const isSubmitting = ref(false);
// Error message for failed operations
const error = ref<string | null>(null);

// ===== LOADING STATE HIERARCHY =====
// 1. isAuthInitialized: Authentication system is ready
// 2. isAuthLoading: User authentication in progress
// 3. isLoading: Organization details loading in progress
// 4. isSubmitting: Form submission in progress

// ===== FORM VALIDATION SCHEMA =====
// Zod schema for form validation with required and optional fields
const formSchema = toTypedSchema(
  z.object({
    name: z.string().min(1, "Organization name is required"),
    description: z.string().optional(),
    address: z.string().optional(),
    phone: z.string().min(1, "Phone number is required"),
    domain: z.string().min(1, "Domain is required"),
    status: z.nativeEnum(OrganizationStatus), // Uses the OrganizationStatus enum
  })
);

// ===== FORM INSTANCE =====
// Initialize form with validation schema and default values
const form = useForm({
  validationSchema: formSchema,
  initialValues: {
    name: "",
    description: "",
    address: "",
    phone: "",
    domain: "",
    status: OrganizationStatus.ACTIVE, // Default to ACTIVE status
  },
});

// ===== METHODS =====

/**
 * Loads organization data from the API
 * Fetches organization details using the selected organization ID
 */
const loadOrganization = async () => {
  // Check if we already have the same organization data loaded FIRST
  if (organization.value?.id === selectedOrganization.value?.id) {
    return;
  }

  // Prevent multiple simultaneous calls
  if (isLoading.value) {
    return;
  }

  // Check if an organization is selected
  if (!selectedOrganization.value?.id) {
    error.value = "No organization selected";
    return;
  }

  isLoading.value = true;
  error.value = null;

  try {
    // Use the composable method instead of calling the service directly
    const orgData = await getOrganizationById(selectedOrganization.value.id);

    if (orgData) {
      organization.value = orgData;
      populateForm(orgData); // Populate form with fetched data
    } else {
      error.value = "Failed to load organization details";
    }
  } catch (err) {
    error.value = "An error occurred while loading organization details";
  } finally {
    isLoading.value = false;
  }
};

/**
 * Populates the form with organization data
 * @param org - Organization data to populate the form with
 */
const populateForm = (org: OrganizationResponse) => {
  const formData = {
    name: org.name || "",
    description: org.description || "",
    address: org.address || "",
    phone: org.phone || "",
    domain: org.domain || "",
    status: org.status || OrganizationStatus.ACTIVE,
  };
  form.setValues(formData);
};

/**
 * Resets the form to the current organization values
 * Useful for discarding unsaved changes
 */
const resetForm = () => {
  if (organization.value) {
    populateForm(organization.value);
  }
};

/**
 * Handles form submission and updates organization data
 * @param values - Form values from validation
 */
const handleSubmit = async (values: any) => {
  // Validation checks
  if (!organization.value) {
    toast.error("No organization selected");
    return;
  }
  if (!user.value?.id) {
    toast.error("No user selected");
    return;
  }

  isSubmitting.value = true;

  try {
    // Prepare update data
    const updateData = {
      name: values.name,
      description: values.description,
      address: values.address,
      phone: values.phone,
      domain: values.domain,
      status: values.status,
    };

    // Use the composable method which handles store and localStorage updates
    const success = await updateOrganization(organization.value.id, user.value.id, updateData);

    if (success) {
      // Refresh the organization data to get the latest from the store
      await loadOrganization();
    }
  } catch (err) {
    toast.error("An error occurred while updating organization");
  } finally {
    isSubmitting.value = false;
  }
};

/**
 * Form submission handler that integrates with vee-validate
 * @param values - Validated form values
 */
const onSubmit = form.handleSubmit(async (values) => {
  await handleSubmit(values);
});

/**
 * Refreshes the organization data
 * Useful for getting the latest data from the server
 */
const refreshOrganization = async () => {
  if (selectedOrganization.value?.id) {
    await loadOrganization();
  }
};

/**
 * Navigates to organization selection page
 * Used when no organization is currently selected
 */
const goToOrganizationSelection = () => {
  router.push("/organization-selection");
};

// ===== LIFECYCLE HOOKS =====
// Load organization data when component mounts
onMounted(() => {
  if (selectedOrganization.value) {
    loadOrganization();
  } else {
    toast.info("No organization selected on mount");
  }
});
</script>
