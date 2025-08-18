<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Change Password</CardTitle>
      <CardDescription class="text-center">
        Enter your current password and choose a new one
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit.prevent="handleSubmit" class="space-y-4">
        <div class="space-y-2">
          <label for="oldPassword" class="text-sm font-medium leading-none">
            Current Password
          </label>
          <Input
            id="oldPassword"
            v-model="form.oldPassword"
            type="password"
            placeholder="Enter your current password"
            required
            class="w-full"
          />
        </div>
        
        <div class="space-y-2">
          <label for="newPassword" class="text-sm font-medium leading-none">
            New Password
          </label>
          <Input
            id="newPassword"
            v-model="form.newPassword"
            type="password"
            placeholder="Enter your new password"
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
          <label for="confirmNewPassword" class="text-sm font-medium leading-none">
            Confirm New Password
          </label>
          <Input
            id="confirmNewPassword"
            v-model="form.confirmNewPassword"
            type="password"
            placeholder="Confirm your new password"
            required
            class="w-full"
            :class="{ 'border-red-500': showPasswordMismatch }"
          />
          <p v-if="showPasswordMismatch" class="text-xs text-red-500">
            Passwords do not match
          </p>
        </div>
        
        <Button type="submit" class="w-full" :disabled="loading || !isFormValid">
          {{ loading ? 'Changing Password...' : 'Change Password' }}
        </Button>
      </form>
    </CardContent>
  </Card>
</template>

<script setup lang="ts">
import { ref, reactive, computed, watch } from 'vue'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'

interface ChangePasswordForm {
  oldPassword: string
  newPassword: string
  confirmNewPassword: string
}

const emit = defineEmits<{
  submit: [form: Omit<ChangePasswordForm, 'confirmNewPassword'>]
  cancel: []
}>()

const loading = ref(false)
const showPasswordMismatch = ref(false)
const form = reactive<ChangePasswordForm>({
  oldPassword: '',
  newPassword: '',
  confirmNewPassword: ''
})

const isFormValid = computed(() => {
  return form.oldPassword.trim() !== '' &&
         form.newPassword.length >= 8 && 
         form.newPassword.length <= 32 &&
         form.newPassword === form.confirmNewPassword
})

// Watch for password mismatch
watch([() => form.newPassword, () => form.confirmNewPassword], ([newPassword, confirmNewPassword]) => {
  if (confirmNewPassword && newPassword !== confirmNewPassword) {
    showPasswordMismatch.value = true
  } else {
    showPasswordMismatch.value = false
  }
})

const handleSubmit = async () => {
  if (!isFormValid.value) {
    if (form.newPassword !== form.confirmNewPassword) {
      showPasswordMismatch.value = true
    }
    return
  }
  
  loading.value = true
  try {
    // Remove confirmNewPassword before sending to backend
    const { confirmNewPassword, ...submitData } = form
    emit('submit', submitData)
  } finally {
    loading.value = false
  }
}
</script>
