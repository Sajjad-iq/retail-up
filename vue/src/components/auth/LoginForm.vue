<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Welcome back</CardTitle>
      <CardDescription class="text-center">
        Enter your credentials to access your account
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit.prevent="handleLogin" class="space-y-4">
        <div class="space-y-2">
          <label for="emailOrPhone" class="text-sm font-medium leading-none">
            Email or Phone
          </label>
          <Input
            id="emailOrPhone"
            v-model="form.emailOrPhone"
            type="text"
            placeholder="Enter your email or phone"
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
            placeholder="Enter your password"
            required
            minlength="8"
            maxlength="32"
            class="w-full"
          />
          <p class="text-xs text-muted-foreground">
            Password must be between 8 and 32 characters
          </p>
        </div>
        
        <Button type="submit" class="w-full" :disabled="loading || !isFormValid">
          {{ loading ? 'Signing in...' : 'Sign in' }}
        </Button>
      </form>
    </CardContent>
    <CardFooter class="flex justify-center">
      <p class="text-sm text-muted-foreground">
        Don't have an account?
        <Button variant="link" @click="$emit('switchToRegister')" class="p-0 h-auto">
          Sign up
        </Button>
      </p>
    </CardFooter>
  </Card>
</template>

<script setup lang="ts">
import { ref, reactive, computed } from 'vue'
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

interface LoginForm {
  emailOrPhone: string
  password: string
}

const emit = defineEmits<{
  switchToRegister: []
  login: [form: LoginForm]
}>()

const loading = ref(false)
const form = reactive<LoginForm>({
  emailOrPhone: '',
  password: ''
})

const isFormValid = computed(() => {
  return form.emailOrPhone.trim() !== '' && 
         form.password.length >= 8 && 
         form.password.length <= 32
})

const handleLogin = async () => {
  if (!isFormValid.value) return
  
  loading.value = true
  try {
    emit('login', { ...form })
  } finally {
    loading.value = false
  }
}
</script>
