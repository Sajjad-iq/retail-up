<template>
  <div
    class="bg-red-50 dark:bg-red-950/20 border border-red-200 dark:border-red-800 rounded-lg p-6"
  >
    <div class="flex items-center gap-3">
      <ExclamationTriangleIcon class="h-6 w-6 text-red-600 dark:text-red-400" />
      <div>
        <h3 class="text-lg font-medium text-red-800 dark:text-red-200">
          Error Loading Inventories
        </h3>
        <p class="text-red-700 dark:text-red-300 mt-1">{{ error }}</p>
      </div>
    </div>
    <Button @click="handleRetry" variant="outline" class="mt-4"> Try Again </Button>
  </div>
</template>

<script setup lang="ts">
import { Button } from "@/components/ui/button";
import { ExclamationTriangleIcon } from "@heroicons/vue/24/outline";

// Props
interface Props {
  error: string;
}

defineProps<Props>();

// Emits
const emit = defineEmits<{
  retry: [];
}>();

// Methods
const handleRetry = () => {
  emit("retry");
};
</script>
