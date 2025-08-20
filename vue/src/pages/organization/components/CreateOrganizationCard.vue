<template>
  <div 
    class="bg-white rounded-xl shadow-lg border-2 border-dashed border-gray-300 hover:border-blue-400 hover:shadow-xl transition-all duration-200 cursor-pointer p-8 text-center aspect-[4/3] w-80 flex flex-col items-center justify-center"
    @click="$emit('click')"
  >
    <div class="w-20 h-20 mx-auto mb-6 bg-blue-100 rounded-full flex items-center justify-center">
      <PlusIcon class="w-10 h-10 text-blue-600" />
    </div>
    <h3 class="text-xl font-semibold text-gray-900 mb-3">Create New Organization</h3>
    <p class="text-gray-600">Start a new business or project</p>
  </div>
</template>

<script setup lang="ts">
import { PlusIcon } from '@heroicons/vue/24/outline'

interface Emits {
  (e: 'click'): void
}

defineEmits<Emits>()
</script>
