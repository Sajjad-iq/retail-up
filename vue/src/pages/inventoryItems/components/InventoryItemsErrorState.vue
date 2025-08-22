<template>
  <div class="bg-card rounded-lg border p-8">
    <div class="text-center">
      <div class="mx-auto h-12 w-12 text-red-400 dark:text-red-300">
        <ExclamationTriangleIcon class="h-12 w-12" />
      </div>
      <h3 class="mt-2 text-sm font-medium text-foreground">Error loading inventory items</h3>
      <p class="mt-1 text-sm text-muted-foreground">
        {{ error?.message || "An unexpected error occurred while loading your inventory items." }}
      </p>
      <div class="mt-6">
        <Button @click="$emit('retry')" variant="outline">
          <ArrowPathIcon class="h-4 w-4 mr-2" />
          Try again
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from "@/components/ui/button";
import { ExclamationTriangleIcon, ArrowPathIcon } from "@heroicons/vue/24/outline";

interface Props {
  error: any;
}

interface Emits {
  (e: "retry"): void;
}

defineProps<Props>();
defineEmits<Emits>();
</script>
