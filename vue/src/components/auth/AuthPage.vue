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
        @login="handleLogin"
      />
      
      <RegisterForm
        v-else
        @switch-to-login="showRegister = false"
        @register="handleRegister"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { useAuthStore } from '@/stores/auth'
import LoginForm from './LoginForm.vue'
import RegisterForm from './RegisterForm.vue'

const emit = defineEmits<{
  'login-success': []
  'register-success': []
}>()

const authStore = useAuthStore()
const showRegister = ref(false)

const handleLogin = async (form: { emailOrPhone: string; password: string }) => {
  const result = await authStore.login(form.emailOrPhone, form.password)
  if (result.success) {
    emit('login-success')
  }
}

const handleRegister = async (form: { name: string; email: string; phone: string; password: string }) => {
  const result = await authStore.register(form.name, form.email, form.phone, form.password)
  if (result.success) {
    emit('register-success')
  }
}
</script>
