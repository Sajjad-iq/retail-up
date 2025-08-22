<template>
  <div class="text-center">
    <button
      @click="$emit('retry')"
      class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-blue-700 dark:text-blue-300 bg-blue-100 dark:bg-blue-950/20 hover:bg-blue-200 dark:hover:bg-blue-900/30 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 dark:focus:ring-blue-400"
    >
      <ArrowPathIcon class="w-4 h-4 mr-2" />
      {{ retryText }}
    </button>
  </div>
</template>

<script setup lang="ts">
import { ArrowPathIcon } from "@heroicons/vue/24/outline";

interface Props {
  retryText?: string;
}

interface Emits {
  (e: "retry"): void;
}

withDefaults(defineProps<Props>(), {
  retryText: "Try Again",
});

defineEmits<Emits>();
</script>
