<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle>{{ mode === 'create' ? 'Create New Item' : 'Edit Item' }}</DialogTitle>
        <DialogDescription>
          {{ mode === 'create' 
            ? 'Add a new item to your inventory with all the necessary details.' 
            : 'Update the item information and settings.'
          }}
        </DialogDescription>
      </DialogHeader>
      
      <form @submit.prevent="handleSubmit" class="space-y-6">
        <!-- Basic Information -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label for="name">Item Name *</Label>
            <Input
              id="name"
              v-model="form.name"
              placeholder="Enter item name"
              required
            />
          </div>
          
          <div class="space-y-2">
            <Label for="sku">SKU</Label>
            <Input
              id="sku"
              v-model="form.sku"
              placeholder="Stock Keeping Unit"
            />
          </div>
        </div>

        <div class="space-y-2">
          <Label for="description">Description</Label>
          <Textarea
            id="description"
            v-model="form.description"
            placeholder="Enter item description"
            rows="3"
          />
        </div>

        <!-- Product Details -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label for="category">Category</Label>
            <Input
              id="category"
              v-model="form.category"
              placeholder="e.g., Electronics"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="brand">Brand</Label>
            <Input
              id="brand"
              v-model="form.brand"
              placeholder="e.g., Apple"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="barcode">Barcode</Label>
            <Input
              id="barcode"
              v-model="form.barcode"
              placeholder="Product barcode"
            />
          </div>
        </div>

        <!-- Stock Information -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label for="currentStock">Current Stock *</Label>
            <Input
              id="currentStock"
              v-model.number="form.currentStock"
              type="number"
              min="0"
              placeholder="0"
              required
            />
          </div>
          
          <div class="space-y-2">
            <Label for="minimumStock">Minimum Stock</Label>
            <Input
              id="minimumStock"
              v-model.number="form.minimumStock"
              type="number"
              min="0"
              placeholder="0"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="unit">Unit *</Label>
            <Select v-model="form.unit">
              <SelectTrigger>
                <SelectValue placeholder="Select unit" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem v-for="unit in availableUnits" :key="unit" :value="unit">
                  {{ unit }}
                </SelectItem>
              </SelectContent>
            </Select>
          </div>
        </div>

        <!-- Pricing -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label for="sellingPrice">Selling Price *</Label>
            <div class="flex">
              <Input
                id="sellingPrice"
                v-model.number="form.sellingPrice.amount"
                type="number"
                min="0"
                step="0.01"
                placeholder="0.00"
                required
                class="rounded-r-none"
              />
              <Select v-model="form.sellingPrice.currency" class="w-24">
                <SelectTrigger class="rounded-l-none border-l-0">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="USD">USD</SelectItem>
                  <SelectItem value="EUR">EUR</SelectItem>
                  <SelectItem value="GBP">GBP</SelectItem>
                </SelectContent>
              </Select>
            </div>
          </div>
          
          <div class="space-y-2">
            <Label for="costPrice">Cost Price</Label>
            <div class="flex">
              <Input
                id="costPrice"
                v-model.number="form.costPrice.amount"
                type="number"
                min="0"
                step="0.01"
                placeholder="0.00"
                class="rounded-r-none"
              />
              <Select v-model="form.costPrice.currency" class="w-24">
                <SelectTrigger class="rounded-l-none border-l-0">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="USD">USD</SelectItem>
                  <SelectItem value="EUR">EUR</SelectItem>
                  <SelectItem value="GBP">GBP</SelectItem>
                </SelectContent>
              </Select>
            </div>
          </div>
        </div>

        <!-- Additional Details -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label for="supplierName">Supplier</Label>
            <Input
              id="supplierName"
              v-model="form.supplierName"
              placeholder="Supplier name"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="productCode">Product Code</Label>
            <Input
              id="productCode"
              v-model="form.productCode"
              placeholder="Internal product code"
            />
          </div>
        </div>

        <!-- Physical Attributes -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label for="weight">Weight (g)</Label>
            <Input
              id="weight"
              v-model.number="form.weight"
              type="number"
              min="0"
              step="0.01"
              placeholder="0.00"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="dimensions">Dimensions</Label>
            <Input
              id="dimensions"
              v-model="form.dimensions"
              placeholder="L x W x H (cm)"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="color">Color</Label>
            <Input
              id="color"
              v-model="form.color"
              placeholder="Item color"
            />
          </div>
        </div>

        <!-- Variants -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label for="size">Size</Label>
            <Input
              id="size"
              v-model="form.size"
              placeholder="Item size"
            />
          </div>
          
          <div class="space-y-2">
            <Label for="isPerishable">Perishable Item</Label>
            <div class="flex items-center space-x-2">
              <Switch
                id="isPerishable"
                v-model="form.isPerishable"
              />
              <Label for="isPerishable">Mark as perishable</Label>
            </div>
          </div>
        </div>

        <!-- Expiry Date (if perishable) -->
        <div v-if="form.isPerishable" class="space-y-2">
          <Label for="expiryDate">Expiry Date</Label>
          <Input
            id="expiryDate"
            v-model="form.expiryDate"
            type="date"
          />
        </div>

        <!-- Status (for edit mode) -->
        <div v-if="mode === 'edit'" class="space-y-2">
          <Label for="isActive">Active Status</Label>
          <div class="flex items-center space-x-2">
            <Switch
              id="isActive"
              v-model="form.isActive"
            />
            <Label for="isActive">Item is active</Label>
          </div>
        </div>

        <!-- Form Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button type="button" variant="outline" @click="$emit('update:open', false)">
            Cancel
          </Button>
          <Button type="submit" :disabled="isSubmitting">
            <span v-if="isSubmitting" class="flex items-center gap-2">
              <div class="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
              {{ mode === 'create' ? 'Creating...' : 'Updating...' }}
            </span>
            <span v-else>{{ mode === 'create' ? 'Create Item' : 'Update Item' }}</span>
          </Button>
        </div>
      </form>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Textarea } from '@/components/ui/textarea'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Switch } from '@/components/ui/switch'
import { useInventoryItems } from '@/composables/useInventoryItems'
import type { CreateInventoryItemRequest, UpdateInventoryItemRequest } from '@/services/inventoryItemService'

interface Props {
  open: boolean
  item?: any
  mode: 'create' | 'edit'
  inventoryId: string
}

interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'success'): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

const { useCreateInventoryItem, useUpdateInventoryItem, useAvailableUnits } = useInventoryItems()

// Queries
const unitsQuery = useAvailableUnits()

// Mutations
const createMutation = useCreateInventoryItem()
const updateMutation = useUpdateInventoryItem()

// Form state
const form = ref<CreateInventoryItemRequest>({
  userId: '',
  inventoryId: props.inventoryId,
  name: '',
  description: '',
  sku: '',
  productCode: '',
  barcode: '',
  category: '',
  brand: '',
  unit: 'PIECES',
  weight: undefined,
  dimensions: '',
  color: '',
  size: '',
  currentStock: 0,
  minimumStock: undefined,
  maximumStock: undefined,
  costPrice: { amount: 0, currency: 'USD' },
  sellingPrice: { amount: 0, currency: 'USD' },
  discountPrice: undefined,
  discountStartDate: undefined,
  discountEndDate: undefined,
  supplierName: '',
  isPerishable: false,
  expiryDate: undefined
})

// Computed
const isSubmitting = computed(() => createMutation.isPending.value || updateMutation.isPending.value)
const availableUnits = computed(() => unitsQuery.data.value?.data || ['PIECES', 'GRAMS', 'KILOGRAMS', 'LITERS'])

// Methods
const resetForm = () => {
  form.value = {
    userId: '',
    inventoryId: props.inventoryId,
    name: '',
    description: '',
    sku: '',
    productCode: '',
    barcode: '',
    category: '',
    brand: '',
    unit: 'PIECES',
    weight: undefined,
    dimensions: '',
    color: '',
    size: '',
    currentStock: 0,
    minimumStock: undefined,
    maximumStock: undefined,
    costPrice: { amount: 0, currency: 'USD' },
    sellingPrice: { amount: 0, currency: 'USD' },
    discountPrice: undefined,
    discountStartDate: undefined,
    discountEndDate: undefined,
    supplierName: '',
    isPerishable: false,
    expiryDate: undefined
  }
}

const handleSubmit = async () => {
  try {
    if (props.mode === 'create') {
      await createMutation.mutateAsync(form.value)
    } else {
      await updateMutation.mutateAsync({
        id: props.item.id,
        itemData: form.value,
        inventoryId: props.inventoryId
      })
    }
    
    emit('success')
    resetForm()
  } catch (error) {
    console.error('Form submission error:', error)
  }
}

// Watchers
watch(() => props.item, (newItem) => {
  if (newItem && props.mode === 'edit') {
    form.value = {
      ...form.value,
      name: newItem.name || '',
      description: newItem.description || '',
      sku: newItem.sku || '',
      productCode: newItem.productCode || '',
      barcode: newItem.barcode || '',
      category: newItem.category || '',
      brand: newItem.brand || '',
      unit: newItem.unit || 'PIECES',
      weight: newItem.weight,
      dimensions: newItem.dimensions || '',
      color: newItem.color || '',
      size: newItem.size || '',
      currentStock: newItem.currentStock || 0,
      minimumStock: newItem.minimumStock,
      maximumStock: newItem.maximumStock,
      costPrice: newItem.costPrice || { amount: 0, currency: 'USD' },
      sellingPrice: newItem.sellingPrice || { amount: 0, currency: 'USD' },
      discountPrice: newItem.discountPrice,
      discountStartDate: newItem.discountStartDate,
      discountEndDate: newItem.discountEndDate,
      supplierName: newItem.supplierName || '',
      isPerishable: newItem.isPerishable || false,
      expiryDate: newItem.expiryDate,
      isActive: newItem.isActive
    }
  } else {
    resetForm()
  }
}, { immediate: true })

watch(() => props.inventoryId, (newId) => {
  form.value.inventoryId = newId
})

// Lifecycle
onMounted(() => {
  resetForm()
})
</script>
