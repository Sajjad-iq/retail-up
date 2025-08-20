<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-[500px]">
      <DialogHeader>
        <DialogTitle>
          {{ mode === 'create' ? 'Create New Inventory' : 'Edit Inventory' }}
        </DialogTitle>
        <DialogDescription>
          {{ mode === 'create' ? 'Add a new inventory location for your organization' : 'Update inventory information' }}
        </DialogDescription>
      </DialogHeader>

      <form @submit="onSubmit" class="space-y-4">
        <!-- Inventory Name -->
        <FormField v-slot="{ componentField }" name="name">
          <FormItem>
            <FormLabel>Inventory Name *</FormLabel>
            <FormControl>
              <Input
                type="text"
                placeholder="e.g., Main Warehouse, Store Location"
                :disabled="isSubmitting"
                v-bind="componentField"
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Description -->
        <FormField v-slot="{ componentField }" name="description">
          <FormItem>
            <FormLabel>Description</FormLabel>
            <FormControl>
              <Textarea
                placeholder="Brief description of this inventory location"
                :disabled="isSubmitting"
                v-bind="componentField"
                rows="3"
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Location -->
        <FormField v-slot="{ componentField }" name="location">
          <FormItem>
            <FormLabel>Location</FormLabel>
            <FormControl>
              <Input
                type="text"
                placeholder="e.g., 123 Main St, City, State"
                :disabled="isSubmitting"
                v-bind="componentField"
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Active Status (only for edit mode) -->
        <FormField v-if="mode === 'edit'" v-slot="{ componentField }" name="isActive">
          <FormItem class="flex flex-row items-center justify-between rounded-lg border p-4">
            <div class="space-y-0.5">
              <FormLabel class="text-base">Active Status</FormLabel>
              <FormDescription>
                Active inventories can be used for operations. Inactive ones are hidden.
              </FormDescription>
            </div>
            <FormControl>
              <Switch
                :disabled="isSubmitting"
                v-bind="componentField"
              />
            </FormControl>
          </FormItem>
        </FormField>

        <!-- Action Buttons -->
        <div class="flex items-center justify-end gap-3 pt-4">
          <Button
            type="button"
            variant="outline"
            @click="$emit('update:open', false)"
            :disabled="isSubmitting"
          >
            Cancel
          </Button>
          <Button
            type="submit"
            :disabled="isSubmitting"
            class="min-w-[100px]"
          >
            <span v-if="isSubmitting" class="flex items-center gap-2">
              <div class="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
              {{ mode === 'create' ? 'Creating...' : 'Updating...' }}
            </span>
            <span v-else>{{ mode === 'create' ? 'Create' : 'Update' }}</span>
          </Button>
        </div>
      </form>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import { ref, watch, onMounted } from 'vue'
import { useForm } from 'vee-validate'
import { toTypedSchema } from '@vee-validate/zod'
import * as z from 'zod'
import { useOrganization } from '@/composables/useOrganization'
import { useInventory } from '@/composables/useInventory'
import type { Inventory } from '@/types/global'

// UI Components
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Textarea } from '@/components/ui/textarea'
import { Switch } from '@/components/ui/switch'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form'

// Props
interface Props {
  open: boolean
  inventory?: Inventory | null
  mode: 'create' | 'edit'
}

// Emits
interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'success', inventory: Inventory): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

// Composables
const { selectedOrganization } = useOrganization()
const { createInventory, updateInventory } = useInventory()

// ===== REACTIVE STATE =====
const isSubmitting = ref(false)

// ===== FORM VALIDATION SCHEMA =====
const formSchema = toTypedSchema(z.object({
  name: z.string()
    .min(2, 'Inventory name must be at least 2 characters')
    .max(100, 'Inventory name must not exceed 100 characters'),
  description: z.string()
    .max(500, 'Description must not exceed 500 characters')
    .optional(),
  location: z.string()
    .max(255, 'Location must not exceed 255 characters')
    .optional(),
  isActive: z.boolean().optional()
}))

// ===== FORM INSTANCE =====
const form = useForm({
  validationSchema: formSchema,
  initialValues: {
    name: '',
    description: '',
    location: '',
    isActive: true
  }
})

// ===== METHODS =====

/**
 * Handle form submission
 */
const onSubmit = form.handleSubmit(async (values) => {
  if (!selectedOrganization.value?.id) {
    return
  }

  isSubmitting.value = true

  try {
    let success = false

    if (props.mode === 'create') {
      // Create new inventory
      success = await createInventory({
        name: values.name,
        description: values.description,
        location: values.location
      })
    } else {
      // Update existing inventory
      if (!props.inventory?.id) {
        return
      }

      success = await updateInventory(props.inventory.id, {
        name: values.name,
        description: values.description,
        location: values.location,
        isActive: values.isActive
      })
    }

    if (success) {
      // The composable handles success toasts and state updates
      // For create mode, we need to emit a success event to close the dialog
      // For edit mode, the inventory is already updated in the composable
      emit('success', props.inventory || {} as Inventory)
    }
  } catch (err) {
    // Error handling is done by the composable
  } finally {
    isSubmitting.value = false
  }
})

/**
 * Populate form with inventory data for editing
 */
const populateForm = (inventory: Inventory) => {
  form.setValues({
    name: inventory.name || '',
    description: inventory.description || '',
    location: inventory.location || '',
    isActive: inventory.isActive ?? true
  })
}

/**
 * Reset form to initial values
 */
const resetForm = () => {
  form.setValues({
    name: '',
    description: '',
    location: '',
    isActive: true
  })
}

// ===== WATCHERS =====
watch(() => props.open, (isOpen) => {
  if (isOpen) {
    if (props.mode === 'edit' && props.inventory) {
      populateForm(props.inventory)
    } else {
      resetForm()
    }
  }
})

// ===== LIFECYCLE HOOKS =====
onMounted(() => {
  if (props.mode === 'edit' && props.inventory) {
    populateForm(props.inventory)
  }
})
</script>
