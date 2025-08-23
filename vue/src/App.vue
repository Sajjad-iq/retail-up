<script setup lang="ts">
import { computed, onMounted } from "vue";
import { SidebarProvider } from "@/components/ui/sidebar";
import MainLayout from "@/components/layout/MainLayout.vue";
import { Toaster } from "@/components/ui/sonner";
import { useAuth } from "@/composables/useAuth";
import { useAuthStore } from "@/stores/auth";
import "vue-sonner/style.css"; // vue-sonner v2 requires this import

const { isAuthenticated, initialize } = useAuth();
const authStore = useAuthStore();
// Show main layout for authenticated users who have selected an organization
// Show organization selection for authenticated users without organization
// Show auth routes without main layout
const showMainLayout = computed(() => {
  return isAuthenticated.value && authStore.hasSelectedOrganization;
});

onMounted(() => {
  initialize();
});
</script>

<template>
  <SidebarProvider>
    <!-- Show main layout for authenticated users who have selected an organization -->
    <MainLayout v-if="showMainLayout">
      <router-view />
    </MainLayout>

    <!-- Show organization selection for authenticated users without organization -->
    <div v-else class="min-h-screen w-full">
      <router-view />
    </div>

    <!-- Toast notifications -->
    <Toaster />
  </SidebarProvider>
</template>

<style>
@import "./assets/main.css";
</style>
