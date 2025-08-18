<template>
  <div class="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
    <div class="w-full max-w-md space-y-8">
      <div class="text-center">
        <h1 class="text-4xl font-bold text-gray-900 mb-2">Retail Up</h1>
        <p class="text-gray-600">Your retail management solution</p>
      </div>
      
      <LoginForm
        v-if="!showRegister"
        @switch-to-register="showRegister = true"
      />
      
      <RegisterForm
        v-else
        @switch-to-login="showRegister = false"
      />
      <Toaster />
    </div>
  </div>
</template>

<script setup lang="ts">
  import LoginForm from './components/LoginForm.vue'
import RegisterForm from './components/RegisterForm.vue'
import { Toaster } from '@/components/ui/sonner'
import { ref } from 'vue'
import 'vue-sonner/style.css' // vue-sonner v2 requires this import

const showRegister = ref(false)
</script>
