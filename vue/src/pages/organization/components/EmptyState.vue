<template>
  <div class="text-center">
    <div
      class="inline-flex items-center px-4 py-2 font-semibold leading-6 text-sm shadow rounded-md text-muted-foreground bg-muted"
    >
      <InformationCircleIcon class="w-5 h-5 mr-2" />
      {{ message }}
    </div>
  </div>
</template>

<script setup lang="ts">
import { InformationCircleIcon } from "@heroicons/vue/24/outline";

interface Props {
  message?: string;
}

withDefaults(defineProps<Props>(), {
  message: "No items found.",
});
</script>
