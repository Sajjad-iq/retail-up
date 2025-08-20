<template>
  <Sheet :open="open" @update:open="$emit('update:open', $event)">
    <SheetContent class="sm:max-w-md p-4 overflow-auto">
      <SheetHeader>
        <SheetTitle class="text-2xl font-bold">Create New Organization</SheetTitle>
        <SheetDescription>
          Fill in the details below to create a new organization.
        </SheetDescription>
      </SheetHeader>

      <form @submit="onSubmit" class="space-y-4 mt-6">
        <!-- Organization Name -->
        <FormField v-slot="{ componentField, errorMessage }" name="name">
          <FormItem>
            <FormLabel>Organization Name</FormLabel>
            <FormControl>
              <Input 
                type="text" 
                placeholder="Enter organization name" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormDescription>
              Name must be between 3 and 255 characters
            </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Domain -->
        <FormField v-slot="{ componentField, errorMessage }" name="domain">
          <FormItem>
            <FormLabel>Domain</FormLabel>
            <FormControl>
              <Input 
                type="text" 
                placeholder="yourdomain.com" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormDescription>
              This will be used as your organization's unique identifier
            </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Description -->
        <FormField v-slot="{ componentField, errorMessage }" name="description">
          <FormItem>
            <FormLabel>Description</FormLabel>
            <FormControl>
              <textarea
                placeholder="Brief description of your organization"
                rows="3"
                class="flex min-h-[80px] w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
                v-bind="componentField"
              ></textarea>
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Address -->
        <FormField v-slot="{ componentField, errorMessage }" name="address">
          <FormItem>
            <FormLabel>Address</FormLabel>
            <FormControl>
              <Input 
                type="text" 
                placeholder="Enter organization address" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Phone -->
        <FormField v-slot="{ componentField, errorMessage }" name="phone">
          <FormLabel>Phone</FormLabel>
          <FormControl>
            <Input 
              type="tel" 
              placeholder="Enter phone number" 
              v-bind="componentField" 
            />
          </FormControl>
          <FormDescription>
            Phone must be between 10 and 20 digits
          </FormDescription>
          <FormMessage />
        </FormField>

        <!-- Email -->
        <FormField v-slot="{ componentField, errorMessage }" name="email">
          <FormItem>
            <FormLabel>Email</FormLabel>
            <FormControl>
              <Input 
                type="email" 
                placeholder="Enter email address" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Form Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button
            type="button"
            variant="outline"
            @click="$emit('update:open', false)"
            :disabled="loading"
          >
            Cancel
          </Button>
          <Button
            type="submit"
            :disabled="loading || !isFormValid"
          >
            {{ loading ? 'Creating...' : 'Create Organization' }}
          </Button>
        </div>
      </form>
    </SheetContent>
  </Sheet>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue'
import { useForm } from 'vee-validate'
import { toTypedSchema } from '@vee-validate/zod'
import * as z from 'zod'
import { useOrganization } from '@/composables/useOrganization'
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle,
} from '@/components/ui/sheet'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form'

interface Props {
  open: boolean
}

interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'organization-created', organization: any): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

const { createOrganization } = useOrganization()
const loading = ref(false)

const formSchema = toTypedSchema(z.object({
  name: z.string().min(3, 'Name must be at least 3 characters').max(255, 'Name must be less than 255 characters'),
  domain: z.string().min(3, 'Domain must be at least 3 characters').max(255, 'Domain must be less than 255 characters'),
  description: z.string().optional(),
  address: z.string().optional(),
  phone: z.string().regex(/^[0-9]+$/, 'Phone must contain only digits').min(10, 'Phone must be at least 10 digits').max(20, 'Phone must be less than 20 digits'),
  email: z.string().email('Please enter a valid email address').optional().or(z.literal('')),
}))

const form = useForm({
  validationSchema: formSchema,
})

// Form validation
const isFormValid = computed(() => {
  return form.values.name?.trim() !== '' && 
         form.values.domain?.trim() !== '' && 
         form.values.phone?.trim() !== ''
})

const handleCreateOrganization = async (formData: { name: string; domain: string; description?: string; address?: string; phone: string; email?: string }) => {
  const result = await createOrganization({
    name: formData.name.trim(),
    domain: formData.domain.trim(),
    description: formData.description?.trim() || undefined,
    address: formData.address?.trim() || undefined,
    phone: formData.phone.trim(),
    email: formData.email?.trim() || undefined
  })

  if (result) {
    // The organization is automatically selected in the composable
    emit('organization-created', result)
    emit('update:open', false)
    form.resetForm()
  }
}

const onSubmit = form.handleSubmit(async (values) => {
  loading.value = true
  try {
    await handleCreateOrganization(values)
  } finally {
    loading.value = false
  }
})
</script>
