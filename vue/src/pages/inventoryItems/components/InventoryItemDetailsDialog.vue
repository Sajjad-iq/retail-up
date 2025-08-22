<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle>Item Details</DialogTitle>
        <DialogDescription> View detailed information about this inventory item </DialogDescription>
      </DialogHeader>

      <div v-if="item" class="space-y-6">
        <!-- Basic Information -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Item Name</Label>
            <p class="text-sm text-foreground">{{ item.name }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">SKU</Label>
            <p class="text-sm text-foreground">{{ item.sku || "Not specified" }}</p>
          </div>
        </div>

        <div class="space-y-2">
          <Label class="text-sm font-medium text-muted-foreground">Description</Label>
          <p class="text-sm text-foreground">{{ item.description || "No description provided" }}</p>
        </div>

        <!-- Product Details -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Category</Label>
            <p class="text-sm text-foreground">{{ item.category || "Not specified" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Brand</Label>
            <p class="text-sm text-foreground">{{ item.brand || "Not specified" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Barcode</Label>
            <p class="text-sm text-foreground">{{ item.barcode || "Not specified" }}</p>
          </div>
        </div>

        <!-- Stock Information -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Current Stock</Label>
            <p class="text-sm text-foreground">{{ item.currentStock }} {{ item.unit }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Minimum Stock</Label>
            <p class="text-sm text-foreground">{{ item.minimumStock || "Not set" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Maximum Stock</Label>
            <p class="text-sm text-foreground">{{ item.maximumStock || "Not set" }}</p>
          </div>
        </div>

        <!-- Pricing -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Selling Price</Label>
            <p class="text-sm text-foreground">
              ${{ item.sellingPrice.amount }} {{ item.sellingPrice.currency }}
            </p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Cost Price</Label>
            <p class="text-sm text-foreground">
              {{
                item.costPrice
                  ? `$${item.costPrice.amount} ${item.costPrice.currency}`
                  : "Not specified"
              }}
            </p>
          </div>
        </div>

        <!-- Additional Details -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Supplier</Label>
            <p class="text-sm text-foreground">{{ item.supplierName || "Not specified" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Product Code</Label>
            <p class="text-sm text-foreground">{{ item.productCode || "Not specified" }}</p>
          </div>
        </div>

        <!-- Physical Attributes -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Weight</Label>
            <p class="text-sm text-foreground">
              {{ item.weight ? `${item.weight}g` : "Not specified" }}
            </p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Dimensions</Label>
            <p class="text-sm text-foreground">{{ item.dimensions || "Not specified" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Color</Label>
            <p class="text-sm text-foreground">{{ item.color || "Not specified" }}</p>
          </div>
        </div>

        <!-- Variants -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Size</Label>
            <p class="text-sm text-foreground">{{ item.size || "Not specified" }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Perishable</Label>
            <p class="text-sm text-foreground">{{ item.isPerishable ? "Yes" : "No" }}</p>
          </div>
        </div>

        <!-- Expiry Date (if perishable) -->
        <div v-if="item.isPerishable && item.expiryDate" class="space-y-2">
          <Label class="text-sm font-medium text-muted-foreground">Expiry Date</Label>
          <p class="text-sm text-foreground">{{ item.expiryDate }}</p>
        </div>

        <!-- Status & Analytics -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Status</Label>
            <Badge :variant="item.isActive ? 'default' : 'secondary'">
              {{ item.isActive ? "Active" : "Inactive" }}
            </Badge>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Total Sold</Label>
            <p class="text-sm text-foreground">{{ item.totalSold || 0 }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Total Revenue</Label>
            <p class="text-sm text-foreground">${{ item.totalRevenue || 0 }}</p>
          </div>
        </div>

        <!-- Timestamps -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Created</Label>
            <p class="text-sm text-foreground">{{ formatDate(item.createdAt) }}</p>
          </div>

          <div class="space-y-2">
            <Label class="text-sm font-medium text-muted-foreground">Last Updated</Label>
            <p class="text-sm text-foreground">{{ formatDate(item.updatedAt) }}</p>
          </div>
        </div>

        <!-- Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button variant="outline" @click="$emit('update:open', false)"> Close </Button>
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
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import { Badge } from "@/components/ui/badge";
import type { InventoryItem } from "@/types/global";
import { formatDate } from "@/lib/utils";

interface Props {
  open: boolean;
  item?: InventoryItem;
}

defineProps<Props>();
</script>
