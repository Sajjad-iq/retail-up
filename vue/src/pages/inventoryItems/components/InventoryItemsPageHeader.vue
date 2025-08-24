<template>
  <div class="flex items-center justify-between mb-8">
    <div>
      <h1 class="text-3xl font-bold text-foreground">Inventory Items</h1>
      <p class="mt-2 text-muted-foreground">
        Manage and track your inventory items across all locations
      </p>
    </div>

    <div class="flex items-center gap-3">
      <!-- Excel Import Button -->
      <Button variant="outline" @click="$emit('import')" class="flex items-center gap-2">
        <Upload class="h-4 w-4" />
        Import Excel
      </Button>

      <!-- Excel Export Button -->
      <Button variant="outline" @click="handleExport" class="flex items-center gap-2">
        <Download class="h-4 w-4" />
        Export Excel
      </Button>

      <!-- Add Item Button -->
      <Button @click="$emit('create')" class="flex items-center gap-2">
        <PlusIcon class="h-4 w-4" />
        Add Item
      </Button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from "@/components/ui/button";
import { PlusIcon } from "@heroicons/vue/24/outline";
import { Upload, Download } from "lucide-vue-next";
import { toast } from "vue-sonner";
import { useCsvExport } from "@/pages/inventoryItems/composeables/useCsvExport";

interface Props {
  items: any[];
  filters: any;
}

const props = defineProps<Props>();

defineEmits<{
  create: [];
  import: [];
}>();

const csvExport = useCsvExport();

const handleExport = () => {
  if (!props.items || props.items.length === 0) {
    toast.error("No items to export");
    return;
  }

  try {
    const csvContent = csvExport.convertToCsv(props.items);
    const filename = csvExport.generateFilename(props.filters);
    csvExport.downloadCsv(csvContent, filename);
    toast.success("Items exported successfully");
  } catch (error) {
    console.error("Export error:", error);
    toast.error("Failed to export items");
  }
};
</script>
