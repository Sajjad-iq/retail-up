<template>
  <div
    class="min-h-screen w-full flex items-center justify-center bg-background py-12 px-4 sm:px-6 lg:px-8"
  >
    <div class="w-full max-w-md space-y-8">
      <div class="text-center">
        <h1 class="text-4xl font-bold text-foreground mb-2">Retail Up</h1>
        <p class="text-muted-foreground">Your retail management solution</p>
      </div>

      <LoginForm v-if="!showRegister" @switch-to-register="showRegister = true" />

      <RegisterForm v-else @switch-to-login="showRegister = false" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from "vue";
import LoginForm from "./components/LoginForm.vue";
import RegisterForm from "./components/RegisterForm.vue";

const showRegister = ref(false);
</script>
