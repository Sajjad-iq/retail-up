<template>
  <div class="flex items-center justify-center py-12">
    <div class="text-center">
      <!-- Spinner with customizable size and color -->
      <div 
        class="animate-spin rounded-full border-b-2 mx-auto"
        :class="[
          sizeClasses[size],
          colorClasses[color]
        ]"
      ></div>
      
      <!-- Optional message below spinner -->
      <p v-if="message" class="mt-4 text-gray-600">{{ message }}</p>
    </div>
  </div>
</template>

<script setup lang="ts">
// Props for customizing the spinner
interface Props {
  /** Size of the spinner (sm, md, lg, xl) */
  size?: 'sm' | 'md' | 'lg' | 'xl'
  /** Color theme of the spinner */
  color?: 'blue' | 'green' | 'purple' | 'red' | 'gray'
  /** Message to display below the spinner */
  message?: string
}

// Default props
const props = withDefaults(defineProps<Props>(), {
  size: 'lg',
  color: 'blue',
  message: ''
})

// Size classes mapping
const sizeClasses = {
  sm: 'h-6 w-6',
  md: 'h-8 w-8',
  lg: 'h-12 w-12',
  xl: 'h-16 w-16'
}

// Color classes mapping
const colorClasses = {
  blue: 'border-blue-600',
  green: 'border-green-600',
  purple: 'border-purple-600',
  red: 'border-red-600',
  gray: 'border-gray-600'
}
</script>
