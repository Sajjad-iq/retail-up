<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Welcome back</CardTitle>
      <CardDescription class="text-center">
        Enter your credentials to access your account
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit="onSubmit" class="space-y-4">
        <FormField v-slot="{ componentField }" name="emailOrPhone">
          <FormItem>
            <FormLabel>Email or Phone</FormLabel>
            <FormControl>
              <Input 
                type="text" 
                placeholder="Enter your email or phone" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>
        
        <FormField v-slot="{ componentField }" name="password">
          <FormItem>
            <FormLabel>Password</FormLabel>
            <FormControl>
              <Input 
                type="password" 
                placeholder="Enter your password" 
                v-bind="componentField" 
              />
            </FormControl>
            <FormDescription>
              Password must be between 8 and 32 characters
            </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>
        
        <Button type="submit" class="w-full" :disabled="loading">
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
import { ref } from 'vue'
import { useForm } from 'vee-validate'
import { toTypedSchema } from '@vee-validate/zod'
import * as z from 'zod'
import { toast } from 'vue-sonner'
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form'
import { useAuthStore } from '@/stores/auth'

interface LoginForm {
  emailOrPhone: string
  password: string
}


const authStore = useAuthStore()
const loading = ref(false)

const handleLogin = async (form: LoginForm): Promise<void> => {
  const result = await authStore.login(form.emailOrPhone, form.password)
  if (result.success) {
    console.log('Login successful')
    toast.success('Login successful!')
  } else {
    // Show error toast
    toast.error(result.error || 'Login failed')
  }
}

const formSchema = toTypedSchema(z.object({
  emailOrPhone: z.string().min(1, 'Email or phone is required'),
  password: z.string().min(8, 'Password must be at least 8 characters').max(32, 'Password must be less than 32 characters')
}))

const form = useForm({
  validationSchema: formSchema,
})

const onSubmit = form.handleSubmit(async (values) => {
  loading.value = true
  try {
    await handleLogin(values)
  } finally {
    loading.value = false
  }
})
</script>
