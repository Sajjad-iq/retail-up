<template>
  <div class="flex items-center justify-between mb-8">
    <div>
      <h1 class="text-3xl font-bold text-foreground">Inventory Items</h1>
      <p class="mt-2 text-muted-foreground">
        Manage and track your inventory items across all locations
      </p>
    </div>

    <div class="flex items-center gap-3">
      <ThemeToggle />
      <Button @click="$emit('create')" class="flex items-center gap-2">
        <PlusIcon class="h-4 w-4" />
        Add Item
      </Button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from "@/components/ui/button";
import { ThemeToggle } from "@/components/ui/theme-toggle";
import { PlusIcon } from "@heroicons/vue/24/outline";

defineEmits<{
  create: [];
}>();
</script>
