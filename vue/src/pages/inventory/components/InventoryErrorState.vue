<template>
  <div class="bg-red-50 border border-red-200 rounded-lg p-6">
    <div class="flex items-center gap-3">
      <ExclamationTriangleIcon class="h-6 w-6 text-red-600" />
      <div>
        <h3 class="text-lg font-medium text-red-800">Error Loading Inventories</h3>
        <p class="text-red-700 mt-1">{{ error }}</p>
      </div>
    </div>
    <Button @click="handleRetry" variant="outline" class="mt-4">
      Try Again
    </Button>
  </div>
</template>

<script setup lang="ts">
import { Button } from '@/components/ui/button'
import { ExclamationTriangleIcon } from '@heroicons/vue/24/outline'

// Props
interface Props {
  error: string
}

defineProps<Props>()

// Emits
const emit = defineEmits<{
  retry: []
}>()

// Methods
const handleRetry = () => {
  emit('retry')
}
</script>
