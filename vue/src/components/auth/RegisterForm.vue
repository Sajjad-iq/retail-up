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
            minlength="3"
            maxlength="255"
            class="w-full"
          />
          <p class="text-xs text-muted-foreground">
            Name must be between 3 and 255 characters
          </p>
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
            minlength="10"
            maxlength="20"
            pattern="[0-9]+"
            class="w-full"
          />
          <p class="text-xs text-muted-foreground">
            Phone must be between 10 and 20 digits
          </p>
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
            minlength="8"
            maxlength="32"
            class="w-full"
          />
          <p class="text-xs text-muted-foreground">
            Password must be between 8 and 32 characters
          </p>
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
            :class="{ 'border-red-500': showPasswordMismatch }"
          />
          <p v-if="showPasswordMismatch" class="text-xs text-red-500">
            Passwords do not match
          </p>
        </div>
        
        <Button type="submit" class="w-full" :disabled="loading || !isFormValid">
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
import { ref, reactive, computed, watch } from 'vue'
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
  register: [form: Omit<RegisterForm, 'confirmPassword'>]
}>()

const loading = ref(false)
const showPasswordMismatch = ref(false)
const form = reactive<RegisterForm>({
  name: '',
  email: '',
  phone: '',
  password: '',
  confirmPassword: ''
})

const isFormValid = computed(() => {
  return form.name.trim().length >= 3 && 
         form.name.trim().length <= 255 &&
         form.email.trim() !== '' &&
         form.phone.length >= 10 && 
         form.phone.length <= 20 &&
         /^[0-9]+$/.test(form.phone) &&
         form.password.length >= 8 && 
         form.password.length <= 32 &&
         form.password === form.confirmPassword
})

// Watch for password mismatch
watch([() => form.password, () => form.confirmPassword], ([password, confirmPassword]) => {
  if (confirmPassword && password !== confirmPassword) {
    showPasswordMismatch.value = true
  } else {
    showPasswordMismatch.value = false
  }
})

const handleRegister = async () => {
  if (!isFormValid.value) {
    if (form.password !== form.confirmPassword) {
      showPasswordMismatch.value = true
    }
    return
  }
  
  loading.value = true
  try {
    // Remove confirmPassword before sending to backend
    const { confirmPassword, ...registerData } = form
    emit('register', registerData)
  } finally {
    loading.value = false
  }
}
</script> 