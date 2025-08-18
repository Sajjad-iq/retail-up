<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Create Organization</CardTitle>
      <CardDescription class="text-center">
        Set up your organization to get started
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit.prevent="handleSubmit" class="space-y-4">
        <div class="space-y-2">
          <label for="name" class="text-sm font-medium leading-none">
            Organization Name
          </label>
          <Input
            id="name"
            v-model="form.name"
            type="text"
            placeholder="Enter organization name"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="domain" class="text-sm font-medium leading-none">
            Domain
          </label>
          <Input
            id="domain"
            v-model="form.domain"
            type="text"
            placeholder="Enter organization domain"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="description" class="text-sm font-medium leading-none">
            Description
          </label>
          <Input
            id="description"
            v-model="form.description"
            type="text"
            placeholder="Brief description of your organization"
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="address" class="text-sm font-medium leading-none">
            Address
          </label>
          <Input
            id="address"
            v-model="form.address"
            type="text"
            placeholder="Organization address"
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
            placeholder="Contact phone number"
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
            placeholder="Organization email"
            class="w-full"
          />
        </div>
        
        <Button type="submit" class="w-full" :disabled="loading">
          {{ loading ? 'Creating...' : 'Create Organization' }}
        </Button>
      </form>
    </CardContent>
  </Card>
</template>

<script setup lang="ts">
import { ref, reactive } from 'vue'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

interface OrganizationForm {
  name: string
  domain: string
  description: string
  address: string
  phone: string
  email: string
}

const emit = defineEmits<{
  submit: [form: OrganizationForm]
}>()

const loading = ref(false)
const form = reactive<OrganizationForm>({
  name: '',
  domain: '',
  description: '',
  address: '',
  phone: '',
  email: ''
})

const handleSubmit = async () => {
  loading.value = true
  try {
    emit('submit', { ...form })
  } finally {
    loading.value = false
  }
}
</script>
