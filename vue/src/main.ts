import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHistory } from 'vue-router'
import { VueQueryPlugin } from '@tanstack/vue-query'
import App from './App.vue'
import { routes } from './router'
import { setupRouterGuards } from './router/guards'
import { queryClient } from './config/query'
import { useThemeStore } from './stores/theme'

const router = createRouter({
    history: createWebHistory(),
    routes
})

// Setup navigation guards
setupRouterGuards(router)

const app = createApp(App)

app.use(createPinia())
app.use(router)
app.use(VueQueryPlugin, { queryClient })

// Initialize theme before mounting
const pinia = app._context.provides.pinia
const themeStore = useThemeStore(pinia)
themeStore.initTheme()

app.mount('#app')
