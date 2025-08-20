import axios, { type AxiosInstance, type AxiosResponse } from 'axios'

class HttpService {
    private static instance: HttpService
    private axiosInstance: AxiosInstance

    private constructor() {
        this.axiosInstance = axios.create({
            baseURL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8082/api',
            timeout: 10000,
            headers: {
                'Content-Type': 'application/json',
            },
        })

        // Request interceptor
        this.axiosInstance.interceptors.request.use(
            (config) => {
                const token = localStorage.getItem('token')
                if (token) {
                    config.headers.Authorization = `Bearer ${token}`
                }
                return config
            },
            (error) => {
                return Promise.reject(error)
            }
        )

        // Response interceptor
        this.axiosInstance.interceptors.response.use(
            (response: AxiosResponse) => {
                return response
            },
            (error) => {
                if (error.response?.status === 401) {
                    // Token expired or invalid
                    localStorage.removeItem('token')
                    localStorage.removeItem('organization')
                    window.location.reload()
                }
                return Promise.reject(error)
            }
        )
    }

    public static getInstance(): HttpService {
        if (!HttpService.instance) {
            HttpService.instance = new HttpService()
        }
        return HttpService.instance
    }

    // Get the axios instance for making requests
    getAxiosInstance(): AxiosInstance {
        return this.axiosInstance
    }

    // Method to update auth token
    setAuthToken(token: string): void {
        this.axiosInstance.defaults.headers.common['Authorization'] = `Bearer ${token}`
    }

    // Method to clear auth token
    clearAuthToken(): void {
        delete this.axiosInstance.defaults.headers.common['Authorization']
    }
}

// Export singleton instance
export const httpService = HttpService.getInstance()
