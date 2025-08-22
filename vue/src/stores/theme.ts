import { defineStore } from 'pinia'
import { ref, watch } from 'vue'

export type Theme = 'dark' | 'light' | 'system'

export const useThemeStore = defineStore('theme', () => {
  const theme = ref<Theme>('system')
  const systemTheme = ref<'dark' | 'light'>('light')

  // Initialize theme from localStorage or system preference
  const initTheme = () => {
    const savedTheme = localStorage.getItem('theme') as Theme
    if (savedTheme && ['dark', 'light', 'system'].includes(savedTheme)) {
      theme.value = savedTheme
    } else {
      // Check system preference
      const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches
      systemTheme.value = prefersDark ? 'dark' : 'light'
    }
    
    applyTheme()
  }

  // Apply the current theme to the document
  const applyTheme = () => {
    const root = window.document.documentElement
    
    if (theme.value === 'system') {
      const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches
      systemTheme.value = prefersDark ? 'dark' : 'light'
      root.classList.toggle('dark', prefersDark)
    } else {
      root.classList.toggle('dark', theme.value === 'dark')
    }
  }

  // Set theme and save to localStorage
  const setTheme = (newTheme: Theme) => {
    theme.value = newTheme
    localStorage.setItem('theme', newTheme)
    applyTheme()
  }

  // Toggle between dark and light (when not in system mode)
  const toggleTheme = () => {
    if (theme.value === 'system') {
      setTheme('light')
    } else if (theme.value === 'light') {
      setTheme('dark')
    } else {
      setTheme('light')
    }
  }

  // Get current effective theme (resolved system preference)
  const getEffectiveTheme = () => {
    if (theme.value === 'system') {
      return systemTheme.value
    }
    return theme.value
  }

  // Watch for system theme changes
  const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)')
  mediaQuery.addEventListener('change', (e) => {
    if (theme.value === 'system') {
      systemTheme.value = e.matches ? 'dark' : 'light'
      applyTheme()
    }
  })

  return {
    theme,
    systemTheme,
    initTheme,
    setTheme,
    toggleTheme,
    getEffectiveTheme,
    applyTheme
  }
})
