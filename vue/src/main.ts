import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHistory } from 'vue-router'
import { VueQueryPlugin } from '@tanstack/vue-query'
import App from './App.vue'
import { routes } from './router'
import { setupRouterGuards } from './router/guards'
import { queryClient } from './config/query'

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

app.mount('#app')
