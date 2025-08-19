import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHistory } from 'vue-router'
import App from './App.vue'
import { routes } from './router'
import { setupRouterGuards } from './router/guards'

const router = createRouter({
    history: createWebHistory(),
    routes
})

// Setup navigation guards
setupRouterGuards(router)

const app = createApp(App)

app.use(createPinia())
app.use(router)

app.mount('#app')
