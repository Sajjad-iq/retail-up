<template>
  <div
    class="bg-card rounded-xl shadow-lg hover:shadow-xl transition-all duration-200 cursor-pointer p-8 border border-border aspect-[4/3] w-80 flex flex-col"
    @click="$emit('click', organization)"
  >
    <div class="flex items-start justify-between mb-6">
      <div
        class="w-16 h-16 bg-gradient-to-br from-blue-500 to-purple-600 rounded-xl flex items-center justify-center text-white font-bold text-xl"
      >
        {{ organization.name.charAt(0).toUpperCase() }}
      </div>
      <Badge :variant="organization.status === 'ACTIVE' ? 'default' : 'secondary'" class="text-xs">
        {{ organization.status }}
      </Badge>
    </div>

    <h3 class="text-xl font-semibold text-foreground mb-3">{{ organization.name }}</h3>
    <p class="text-muted-foreground mb-4 flex-grow">
      {{ organization.description || "No description" }}
    </p>

    <div class="space-y-2 text-sm text-muted-foreground mt-auto">
      <div class="flex items-center">
        <GlobeAltIcon class="w-4 h-4 mr-3" />
        {{ organization.domain }}
      </div>
      <div class="flex items-center">
        <PhoneIcon class="w-4 h-4 mr-3" />
        {{ organization.phone }}
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { Badge } from "@/components/ui/badge";
import { GlobeAltIcon, PhoneIcon } from "@heroicons/vue/24/outline";
import type { Organization } from "@/types/global";

interface Props {
  organization: Organization;
}

interface Emits {
  (e: "click", organization: Organization): void;
}

defineProps<Props>();
defineEmits<Emits>();
</script>
