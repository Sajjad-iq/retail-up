<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-4xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle class="text-2xl font-semibold flex items-center gap-2">
          <Package class="h-6 w-6 text-primary" />
          Item Details
        </DialogTitle>
        <DialogDescription class="text-base">
          Comprehensive information about this inventory item
        </DialogDescription>
      </DialogHeader>

      <div v-if="item" class="space-y-8">
        <!-- Basic Information Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Info class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Basic Information</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Item Name</Label>
              <p class="text-base font-medium text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.name }}
              </p>
            </div>
          </div>

          <div class="space-y-3 mt-4">
            <Label class="text-sm font-medium text-muted-foreground">Description</Label>
            <p class="text-base text-foreground p-3 bg-muted/50 rounded-md min-h-[60px]">
              {{ item.description || "No description provided" }}
            </p>
          </div>
        </div>

        <!-- Product Details Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Tag class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Product Details</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Category</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.category || "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Brand</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.brand || "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Barcode</Label>
              <p class="text-base font-mono text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.barcode || "Not specified" }}
              </p>
            </div>
          </div>
        </div>

        <!-- Stock Information Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Warehouse class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Stock Information</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Current Stock</Label>
              <div class="flex items-center gap-2">
                <div class="p-3 bg-muted/50 rounded-md flex-1">
                  <span class="text-2xl font-bold text-foreground">{{ item.currentStock }}</span>
                  <span class="text-sm text-muted-foreground ml-2">{{ item.unit }}</span>
                </div>
                <Badge
                  :variant="
                    item.currentStock > (item.minimumStock || 0) ? 'default' : 'destructive'
                  "
                >
                  {{ item.currentStock > (item.minimumStock || 0) ? "In Stock" : "Low Stock" }}
                </Badge>
              </div>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Minimum Stock</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.minimumStock || "Not set" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Maximum Stock</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.maximumStock || "Not set" }}
              </p>
            </div>
          </div>
        </div>

        <!-- Pricing Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <DollarSign class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Pricing</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Selling Price</Label>
              <div class="p-3 bg-green-50 border border-green-200 rounded-md">
                <span class="text-2xl font-bold text-green-700"
                  >${{ item.sellingPrice.amount }}</span
                >
                <span class="text-sm text-green-600 ml-2">{{ item.sellingPrice.currency }}</span>
              </div>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Cost Price</Label>
              <div v-if="item.costPrice" class="p-3 bg-blue-50 border border-blue-200 rounded-md">
                <span class="text-2xl font-bold text-blue-700">${{ item.costPrice.amount }}</span>
                <span class="text-sm text-blue-600 ml-2">{{ item.costPrice.currency }}</span>
              </div>
              <p v-else class="text-base text-muted-foreground p-3 bg-muted/50 rounded-md">
                Not specified
              </p>
            </div>
          </div>
        </div>

        <!-- Additional Details Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <FileText class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Additional Details</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Supplier</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.supplierName || "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Product Code</Label>
              <p class="text-base font-mono text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.productCode || "Not specified" }}
              </p>
            </div>
          </div>
        </div>

        <!-- Physical Attributes Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Ruler class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Physical Attributes</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Weight</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.weight ? `${item.weight}g` : "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Dimensions</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.dimensions || "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Color</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.color || "Not specified" }}
              </p>
            </div>
          </div>
        </div>

        <!-- Variants Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Shapes class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Variants & Properties</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Size</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ item.size || "Not specified" }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Perishable</Label>
              <div class="p-3 bg-muted/50 rounded-md">
                <Badge :variant="item.isPerishable ? 'destructive' : 'secondary'">
                  {{ item.isPerishable ? "Yes" : "No" }}
                </Badge>
              </div>
            </div>
          </div>

          <!-- Expiry Date (if perishable) -->
          <div v-if="item.isPerishable && item.expiryDate" class="mt-4 space-y-3">
            <Label class="text-sm font-medium text-muted-foreground">Expiry Date</Label>
            <p
              class="text-base text-foreground p-3 bg-red-50 border border-red-200 rounded-md text-red-700"
            >
              {{ item.expiryDate }}
            </p>
          </div>
        </div>

        <!-- Status & Analytics Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <BarChart3 class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Status & Analytics</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Status</Label>
              <div class="p-3 bg-muted/50 rounded-md">
                <Badge :variant="item.isActive ? 'default' : 'secondary'" class="text-sm">
                  {{ item.isActive ? "Active" : "Inactive" }}
                </Badge>
              </div>
            </div>
          </div>
        </div>

        <!-- Timestamps Card -->
        <div class="bg-card border rounded-lg p-6">
          <div class="flex items-center gap-2 mb-4">
            <Clock class="h-5 w-5 text-primary" />
            <h3 class="text-lg font-semibold">Timestamps</h3>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Created</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ formatDate(item.createdAt) }}
              </p>
            </div>

            <div class="space-y-3">
              <Label class="text-sm font-medium text-muted-foreground">Last Updated</Label>
              <p class="text-base text-foreground p-3 bg-muted/50 rounded-md">
                {{ formatDate(item.updatedAt) }}
              </p>
            </div>
          </div>
        </div>

        <!-- Actions -->
        <div class="flex justify-end space-x-3 pt-6 border-t">
          <Button variant="outline" @click="$emit('update:open', false)" class="px-6">
            <X class="h-4 w-4 mr-2" />
            Close
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
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import { Badge } from "@/components/ui/badge";
import type { InventoryItem } from "@/types/global";
import { formatDate } from "@/lib/utils";
import {
  Package,
  Info,
  Tag,
  Warehouse,
  DollarSign,
  FileText,
  Ruler,
  Shapes,
  BarChart3,
  Clock,
  X,
} from "lucide-vue-next";

interface Props {
  open: boolean;
  item?: InventoryItem;
}

defineProps<Props>();
</script>
