<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle>{{ mode === "create" ? "Create New Item" : "Edit Item" }}</DialogTitle>
        <DialogDescription>
          {{
            mode === "create"
              ? "Add a new item to your inventory with all the necessary details."
              : "Update the item information and settings."
          }}
        </DialogDescription>
      </DialogHeader>

      <form @submit="onSubmit" class="space-y-6">
        <!-- Basic Information -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="name">
            <FormItem>
              <FormLabel>Item Name *</FormLabel>
              <FormControl>
                <Input placeholder="Enter item name" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="sku">
            <FormItem>
              <FormLabel>SKU</FormLabel>
              <FormControl>
                <Input placeholder="Stock Keeping Unit" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <FormField v-slot="{ componentField, errorMessage }" name="description">
          <FormItem>
            <FormLabel>Description</FormLabel>
            <FormControl>
              <Textarea placeholder="Enter item description" rows="3" v-bind="componentField" />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Product Details -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="category">
            <FormItem>
              <FormLabel>Category</FormLabel>
              <FormControl>
                <Input placeholder="e.g., Electronics" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="brand">
            <FormItem>
              <FormLabel>Brand</FormLabel>
              <FormControl>
                <Input placeholder="e.g., Apple" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="barcode">
            <FormItem>
              <FormLabel>Barcode</FormLabel>
              <FormControl>
                <Input placeholder="Product barcode" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Stock Information -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="currentStock">
            <FormItem>
              <FormLabel>Current Stock *</FormLabel>
              <FormControl>
                <Input type="number" min="0" placeholder="0" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="minimumStock">
            <FormItem>
              <FormLabel>Minimum Stock</FormLabel>
              <FormControl>
                <Input type="number" min="0" placeholder="0" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <!-- Unit Selection -->
          <FormField v-slot="{ componentField, errorMessage }" name="unit">
            <FormItem>
              <FormLabel>Unit *</FormLabel>
              <Select v-bind="componentField">
                <FormControl>
                  <SelectTrigger>
                    <SelectValue placeholder="Select unit" />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  <SelectItem v-for="unit in availableUnits" :key="unit" :value="unit">
                    {{ unit }}
                  </SelectItem>
                </SelectContent>
              </Select>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Pricing -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="sellingPrice">
            <FormItem>
              <FormLabel>Selling Price *</FormLabel>
              <FormControl>
                <div class="flex">
                  <Input
                    type="number"
                    min="0"
                    step="0.01"
                    placeholder="0.00"
                    class="rounded-r-none"
                    v-bind="componentField"
                  />
                  <Select v-model="sellingPriceCurrency">
                    <SelectTrigger class="rounded-l-none border-l-0 w-24">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem :value="CurrencyEnum.USD">{{ CurrencyEnum.USD }}</SelectItem>
                      <SelectItem :value="CurrencyEnum.EUR">{{ CurrencyEnum.EUR }}</SelectItem>
                      <SelectItem :value="CurrencyEnum.GBP">{{ CurrencyEnum.GBP }}</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="costPrice">
            <FormItem>
              <FormLabel>Cost Price</FormLabel>
              <FormControl>
                <div class="flex">
                  <Input
                    type="number"
                    min="0"
                    step="0.01"
                    placeholder="0.00"
                    class="rounded-r-none"
                    v-bind="componentField"
                  />
                  <Select v-model="costPriceCurrency">
                    <SelectTrigger class="rounded-l-none border-l-0 w-24">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem :value="CurrencyEnum.USD">{{ CurrencyEnum.USD }}</SelectItem>
                      <SelectItem :value="CurrencyEnum.EUR">{{ CurrencyEnum.EUR }}</SelectItem>
                      <SelectItem :value="CurrencyEnum.GBP">{{ CurrencyEnum.GBP }}</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Additional Details -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="supplierName">
            <FormItem>
              <FormLabel>Supplier</FormLabel>
              <FormControl>
                <Input placeholder="Supplier name" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="productCode">
            <FormItem>
              <FormLabel>Product Code</FormLabel>
              <FormControl>
                <Input placeholder="Internal product code" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Physical Attributes -->
        <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="weight">
            <FormItem>
              <FormLabel>Weight (g)</FormLabel>
              <FormControl>
                <Input
                  type="number"
                  min="0"
                  step="0.01"
                  placeholder="0.00"
                  v-bind="componentField"
                />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="dimensions">
            <FormItem>
              <FormLabel>Dimensions</FormLabel>
              <FormControl>
                <Input placeholder="L x W x H (cm)" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="color">
            <FormItem>
              <FormLabel>Color</FormLabel>
              <FormControl>
                <Input placeholder="Item color" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Variants -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <FormField v-slot="{ componentField, errorMessage }" name="size">
            <FormItem>
              <FormLabel>Size</FormLabel>
              <FormControl>
                <Input placeholder="Item size" v-bind="componentField" />
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>

          <FormField v-slot="{ componentField, errorMessage }" name="isPerishable">
            <FormItem>
              <FormLabel>Perishable Item</FormLabel>
              <FormControl>
                <div class="flex items-center space-x-2">
                  <Switch
                    v-model="componentField.modelValue"
                    :checked="componentField.modelValue"
                    @update:model-value="(value: boolean) => componentField.onChange(value)"
                  />
                  <Label>Mark as perishable</Label>
                </div>
              </FormControl>
              <FormMessage />
            </FormItem>
          </FormField>
        </div>

        <!-- Expiry Date (if perishable) -->
        <FormField
          v-slot="{ componentField, errorMessage }"
          name="expiryDate"
          v-if="formValues.isPerishable"
        >
          <FormItem>
            <FormLabel>Expiry Date</FormLabel>
            <FormControl>
              <Input type="date" v-bind="componentField" />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Status (for edit mode) -->
        <FormField v-slot="{ componentField, errorMessage }" name="isActive" v-if="mode === 'edit'">
          <FormItem>
            <FormLabel>Active Status</FormLabel>
            <FormControl>
              <div class="flex items-center space-x-2">
                <Switch
                  v-model="componentField.modelValue"
                  :checked="componentField.modelValue"
                  @update:model-value="(value: boolean) => componentField.onChange(value)"
                />
                <Label>Item is active</Label>
              </div>
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <!-- Form Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button type="button" variant="outline" @click="$emit('update:open', false)">
            Cancel
          </Button>
          <Button type="submit" :disabled="isSubmitting">
            <span v-if="isSubmitting" class="flex items-center gap-2">
              <div class="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
              {{ mode === "create" ? "Creating..." : "Updating..." }}
            </span>
            <span v-else>{{ mode === "create" ? "Create Item" : "Update Item" }}</span>
          </Button>
        </div>
      </form>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import { ref, computed, watch, onMounted, nextTick } from "vue";
import { useForm } from "vee-validate";
import { toTypedSchema } from "@vee-validate/zod";
import * as z from "zod";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Textarea } from "@/components/ui/textarea";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Switch } from "@/components/ui/switch";
import { FormControl, FormField, FormItem, FormLabel, FormMessage } from "@/components/ui/form";
import { useInventoryItems } from "@/composables/useInventoryItems";
import type {
  CreateInventoryItemRequest,
  UpdateInventoryItemRequest,
} from "@/services/inventoryItemService";
import type { Currency } from "@/types/global";
import { Unit, Currency as CurrencyEnum } from "@/types/global";

interface Props {
  open: boolean;
  item?: any;
  mode: "create" | "edit";
  inventoryId: string;
}

interface Emits {
  (e: "update:open", value: boolean): void;
  (e: "success"): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const { useCreateInventoryItem, useUpdateInventoryItem, useAvailableUnits } = useInventoryItems();

// Queries
const unitsQuery = useAvailableUnits();

// Mutations
const createMutation = useCreateInventoryItem();
const updateMutation = useUpdateInventoryItem();

// Form validation schema
const formSchema = toTypedSchema(
  z.object({
    name: z
      .string()
      .min(2, "Item name must be at least 2 characters")
      .max(200, "Name must be less than 200 characters"),
    description: z.string().max(1000, "Description must not exceed 1000 characters").optional(),
    sku: z
      .string()
      .regex(/^[A-Za-z0-9_-]+$/, "SKU must contain only letters, numbers, hyphens, and underscores")
      .max(50, "SKU must not exceed 50 characters")
      .optional(),
    productCode: z
      .string()
      .regex(
        /^[A-Za-z0-9_-]+$/,
        "Product code must contain only letters, numbers, hyphens, and underscores"
      )
      .max(50, "Product code must not exceed 50 characters")
      .optional(),
    barcode: z
      .string()
      .regex(
        /^[A-Za-z0-9_-]+$/,
        "Barcode must contain only letters, numbers, hyphens, and underscores"
      )
      .max(100, "Barcode must not exceed 100 characters")
      .optional(),
    category: z.string().max(100, "Category must not exceed 100 characters").optional(),
    brand: z.string().max(100, "Brand must not exceed 100 characters").optional(),
    unit: z.nativeEnum(Unit),
    weight: z.number().min(0, "Weight must be positive").optional(),
    dimensions: z.string().max(50, "Dimensions must not exceed 50 characters").optional(),
    color: z.string().max(50, "Color must not exceed 50 characters").optional(),
    size: z.string().max(20, "Size must not exceed 20 characters").optional(),
    currentStock: z.number().min(0, "Current stock must be positive"),
    minimumStock: z.number().min(0, "Minimum stock must be positive").optional(),
    maximumStock: z.number().min(0, "Maximum stock must be positive").optional(),
    sellingPrice: z.number().min(0, "Selling price must be positive"),
    costPrice: z.number().min(0, "Cost price must be positive").optional(),
    supplierName: z.string().max(200, "Supplier name must not exceed 200 characters").optional(),
    isPerishable: z.boolean(),
    expiryDate: z.string().optional(),
    isActive: z.boolean(),
  })
);

// Form instance
const form = useForm({
  validationSchema: formSchema,
  initialValues: {
    name: "",
    description: "",
    sku: "",
    productCode: "",
    barcode: "",
    category: "",
    brand: "",
    unit: Unit.PIECES,
    weight: undefined,
    dimensions: "",
    color: "",
    size: "",
    currentStock: 0,
    minimumStock: undefined,
    maximumStock: undefined,
    sellingPrice: 0,
    costPrice: undefined,
    supplierName: "",
    isPerishable: false,
    expiryDate: undefined,
    isActive: true,
  },
});

// Computed
const isSubmitting = computed(
  () => createMutation.isPending.value || updateMutation.isPending.value
);
const availableUnits = computed(
  () => unitsQuery.data.value?.data || [Unit.PIECES, Unit.GRAMS, Unit.KILOGRAMS, Unit.LITERS]
);
const formValues = computed(() => form.values);

// Currency state
const sellingPriceCurrency = ref(CurrencyEnum.USD);
const costPriceCurrency = ref(CurrencyEnum.USD);

// Methods
const resetForm = () => {
  form.resetForm();
  sellingPriceCurrency.value = CurrencyEnum.USD;
  costPriceCurrency.value = CurrencyEnum.USD;
};

const handleSubmit = async (values: any) => {
  try {
    const formData: CreateInventoryItemRequest = {
      userId: "", // Will be set by the service
      inventoryId: props.inventoryId,
      name: values.name,
      description: values.description,
      sku: values.sku,
      productCode: values.productCode,
      barcode: values.barcode,
      category: values.category,
      brand: values.brand,
      unit: values.unit,
      weight: values.weight,
      dimensions: values.dimensions,
      color: values.color,
      size: values.size,
      currentStock: values.currentStock,
      minimumStock: values.minimumStock,
      maximumStock: values.maximumStock,
      costPrice: values.costPrice
        ? { amount: values.costPrice, currency: costPriceCurrency.value as Currency }
        : undefined,
      sellingPrice: {
        amount: values.sellingPrice,
        currency: sellingPriceCurrency.value as Currency,
      },
      discountPrice: undefined,
      discountStartDate: undefined,
      discountEndDate: undefined,
      supplierName: values.supplierName,
      isPerishable: values.isPerishable,
      expiryDate: values.expiryDate,
    };

    if (props.mode === "create") {
      await createMutation.mutateAsync(formData);
    } else {
      await updateMutation.mutateAsync({
        id: props.item.id,
        itemData: formData,
        inventoryId: props.inventoryId,
      });
    }

    emit("success");
    resetForm();
  } catch (error) {
    console.error("Form submission error:", error);
  }
};

const onSubmit = form.handleSubmit(handleSubmit);

// Watchers
watch(
  () => props.item,
  (newItem) => {
    if (newItem && props.mode === "edit") {
      // Use nextTick to ensure form is fully initialized
      nextTick(() => {
        populateFormWithItem(newItem);
      });
    } else {
      resetForm();
    }
  },
  { immediate: true, deep: true }
);

watch(
  () => props.inventoryId,
  (newId) => {
    // Update form when inventory changes
  }
);

// Lifecycle
onMounted(() => {
  resetForm();

  // If we're in edit mode and have an item, populate the form
  if (props.mode === "edit" && props.item) {
    nextTick(() => {
      populateFormWithItem(props.item);
    });
  }
});

// Helper function to populate form with item data
const populateFormWithItem = (item: any) => {
  form.setValues({
    name: item.name || "",
    description: item.description || "",
    sku: item.sku || "",
    productCode: item.productCode || "",
    barcode: item.barcode || "",
    category: item.category || "",
    brand: item.brand || "",
    unit: item.unit || Unit.PIECES,
    weight: item.weight || undefined,
    dimensions: item.dimensions || "",
    color: item.color || "",
    size: item.size || "",
    currentStock: item.currentStock || 0,
    minimumStock: item.minimumStock || undefined,
    maximumStock: item.maximumStock || undefined,
    sellingPrice: item.sellingPrice?.amount || 0,
    costPrice: item.costPrice?.amount || undefined,
    supplierName: item.supplierName || "",
    isPerishable: item.isPerishable || false,
    expiryDate: item.expiryDate || undefined,
    isActive: item.isActive !== undefined ? item.isActive : true,
  });

  if (item.sellingPrice?.currency) {
    sellingPriceCurrency.value = item.sellingPrice.currency;
  }
  if (item.costPrice?.currency) {
    costPriceCurrency.value = item.costPrice.currency;
  }
};
</script>
