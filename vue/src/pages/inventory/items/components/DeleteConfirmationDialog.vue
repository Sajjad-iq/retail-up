<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-md">
      <DialogHeader>
        <DialogTitle>Delete inventory item</DialogTitle>
        <DialogDescription>
          Are you sure you want to delete <span class="font-medium">{{ itemName }}</span>? 
          This action cannot be undone.
        </DialogDescription>
      </DialogHeader>
      
      <div class="flex justify-end space-x-2 pt-4">
        <Button variant="outline" @click="$emit('update:open', false)">
          Cancel
        </Button>
        <Button variant="destructive" @click="$emit('confirm')">
          Delete
        </Button>
      </div>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'

interface Props {
  open: boolean
  itemName?: string
}

interface Emits {
  (e: 'update:open', value: boolean): void
  (e: 'confirm'): void
}

defineProps<Props>()
defineEmits<Emits>()
</script>
