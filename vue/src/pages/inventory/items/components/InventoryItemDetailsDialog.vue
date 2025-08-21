<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle>Item Details</DialogTitle>
        <DialogDescription>
          View detailed information about this inventory item
        </DialogDescription>
      </DialogHeader>
      
      <div v-if="item" class="space-y-6">
        <!-- Basic Information -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Item Name</Label>
            <p class="text-sm text-gray-900">{{ item.name }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">SKU</Label>
            <p class="text-sm text-gray-900">{{ item.sku || 'Not specified' }}</p>
          </div>
        </div>

        <div class="space-y-2">
          <Label class="text-sm font-medium text-gray-500">Description</Label>
          <p class="text-sm text-gray-900">{{ item.description || 'No description provided' }}</p>
        </div>

        <!-- Product Details -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Category</Label>
            <p class="text-sm text-gray-900">{{ item.category || 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Brand</Label>
            <p class="text-sm text-gray-900">{{ item.brand || 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Barcode</Label>
            <p class="text-sm text-gray-900">{{ item.barcode || 'Not specified' }}</p>
          </div>
        </div>

        <!-- Stock Information -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Current Stock</Label>
            <p class="text-sm text-gray-900">{{ item.currentStock }} {{ item.unit }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Minimum Stock</Label>
            <p class="text-sm text-gray-900">{{ item.minimumStock || 'Not set' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Maximum Stock</Label>
            <p class="text-sm text-gray-900">{{ item.maximumStock || 'Not set' }}</p>
          </div>
        </div>

        <!-- Pricing -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Selling Price</Label>
            <p class="text-sm text-gray-900">${{ item.sellingPrice.amount }} {{ item.sellingPrice.currency }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Cost Price</Label>
            <p class="text-sm text-gray-900">
              {{ item.costPrice ? `$${item.costPrice.amount} ${item.costPrice.currency}` : 'Not specified' }}
            </p>
          </div>
        </div>

        <!-- Additional Details -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Supplier</Label>
            <p class="text-sm text-gray-900">{{ item.supplierName || 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Product Code</Label>
            <p class="text-sm text-gray-900">{{ item.productCode || 'Not specified' }}</p>
          </div>
        </div>

        <!-- Physical Attributes -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Weight</Label>
            <p class="text-sm text-gray-900">{{ item.weight ? `${item.weight}g` : 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Dimensions</Label>
            <p class="text-sm text-gray-900">{{ item.dimensions || 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Color</Label>
            <p class="text-sm text-gray-900">{{ item.color || 'Not specified' }}</p>
          </div>
        </div>

        <!-- Variants -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Size</Label>
            <p class="text-sm text-gray-900">{{ item.size || 'Not specified' }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Perishable</Label>
            <p class="text-sm text-gray-900">{{ item.isPerishable ? 'Yes' : 'No' }}</p>
          </div>
        </div>

        <!-- Expiry Date (if perishable) -->
        <div v-if="item.isPerishable && item.expiryDate" class="space-y-2">
          <Label class="text-sm font-medium text-gray-500">Expiry Date</Label>
          <p class="text-sm text-gray-900">{{ item.expiryDate }}</p>
        </div>

        <!-- Status & Analytics -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Status</Label>
            <Badge :variant="item.isActive ? 'default' : 'secondary'">
              {{ item.isActive ? 'Active' : 'Inactive' }}
            </Badge>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Total Sold</Label>
            <p class="text-sm text-gray-900">{{ item.totalSold || 0 }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Total Revenue</Label>
            <p class="text-sm text-gray-900">${{ item.totalRevenue || 0 }}</p>
          </div>
        </div>

        <!-- Timestamps -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Created</Label>
            <p class="text-sm text-gray-900">{{ formatDate(item.createdAt) }}</p>
          </div>
          
          <div class="space-y-2">
            <Label class="text-sm font-medium text-gray-500">Last Updated</Label>
            <p class="text-sm text-gray-900">{{ formatDate(item.updatedAt) }}</p>
          </div>
        </div>

        <!-- Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button variant="outline" @click="$emit('update:open', false)">
            Close
          </Button>
          <Button @click="$emit('edit', item)">
            <PencilIcon class="h-4 w-4 mr-2" />
            Edit Item
          </Button>
        </div>
      </div>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import { Badge } from '@/components/ui/badge'
import { PencilIcon } from '@heroicons/vue/24/outline'

interface Props {
  open: boolean
  item?: any
}

interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'edit', item: any): void
}

defineProps<Props>()
defineEmits<Emits>()

const formatDate = (dateString: string) => {
  if (!dateString) return 'Not available'
  return new Date(dateString).toLocaleDateString()
}
</script>
