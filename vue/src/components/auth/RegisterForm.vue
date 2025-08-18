<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Create account</CardTitle>
      <CardDescription class="text-center">
        Enter your details to create a new account
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit.prevent="handleRegister" class="space-y-4">
        <div class="space-y-2">
          <label for="name" class="text-sm font-medium leading-none">
            Full Name
          </label>
          <Input
            id="name"
            v-model="form.name"
            type="text"
            placeholder="Enter your full name"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="email" class="text-sm font-medium leading-none">
            Email
          </label>
          <Input
            id="email"
            v-model="form.email"
            type="email"
            placeholder="Enter your email"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="phone" class="text-sm font-medium leading-none">
            Phone
          </label>
          <Input
            id="phone"
            v-model="form.phone"
            type="tel"
            placeholder="Enter your phone number"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="password" class="text-sm font-medium leading-none">
            Password
          </label>
          <Input
            id="password"
            v-model="form.password"
            type="password"
            placeholder="Create a password"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="confirmPassword" class="text-sm font-medium leading-none">
            Confirm Password
          </label>
          <Input
            id="confirmPassword"
            v-model="form.confirmPassword"
            type="password"
            placeholder="Confirm your password"
            required
            class="w-full"
          />
        </div>
        
        <Button type="submit" class="w-full" :disabled="loading">
          {{ loading ? 'Creating account...' : 'Create account' }}
        </Button>
      </form>
    </CardContent>
    <CardFooter class="flex justify-center">
      <p class="text-sm text-muted-foreground">
        Already have an account?
        <Button variant="link" @click="$emit('switchToLogin')" class="p-0 h-auto">
          Sign in
        </Button>
      </p>
    </CardFooter>
  </Card>
</template>

<script setup lang="ts">
import { ref, reactive } from 'vue'
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

interface RegisterForm {
  name: string
  email: string
  phone: string
  password: string
  confirmPassword: string
}

const emit = defineEmits<{
  switchToLogin: []
  register: [form: RegisterForm]
}>()

const loading = ref(false)
const form = reactive<RegisterForm>({
  name: '',
  email: '',
  phone: '',
  password: '',
  confirmPassword: ''
})

const handleRegister = async () => {
  if (form.password !== form.confirmPassword) {
    // Handle password mismatch
    return
  }
  
  loading.value = true
  try {
    emit('register', { ...form })
  } finally {
    loading.value = false
  }
}
</script> 