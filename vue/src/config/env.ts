export const config = {
    api: {
        baseURL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8082/api',
    },
    app: {
        name: import.meta.env.VITE_APP_NAME || 'Retail Up',
        version: import.meta.env.VITE_APP_VERSION || '1.0.0',
    },
} as const
