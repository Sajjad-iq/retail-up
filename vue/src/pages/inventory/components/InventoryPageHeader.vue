<template>
  <div class="mb-8">
    <div class="flex items-center justify-between">
      <div>
        <h1 class="text-3xl font-bold text-foreground">Inventory Management</h1>
        <p class="mt-2 text-muted-foreground">
          Manage your organization's inventories and storage locations
        </p>
      </div>
      <div class="flex items-center gap-3">
        <Button @click="handleCreate" class="flex items-center gap-2">
          <PlusIcon class="h-5 w-5" />
          Create Inventory
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Button } from "@/components/ui/button";
import { ThemeToggle } from "@/components/ui/theme-toggle";
import { PlusIcon } from "@heroicons/vue/24/outline";

// Emits
const emit = defineEmits<{
  create: [];
}>();

// Methods
const handleCreate = () => {
  emit("create");
};
</script>
