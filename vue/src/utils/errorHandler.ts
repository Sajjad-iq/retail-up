export interface ApiError {
    message: string
    status?: number
    code?: string
    error?: string
    fieldErrors?: Record<string, string>
}

export class ErrorHandler {
    static handleApiError(error: any): ApiError {
        if (error.response) {
            // Server responded with error status
            const { status, data } = error.response

            // Handle the backend GlobalExceptionHandler format
            if (data && typeof data === 'object') {
                return {
                    message: data.message || `Request failed with status ${status}`,
                    status,
                    code: data.error || `HTTP_${status}`,
                    error: data.error,
                    fieldErrors: data.fieldErrors
                }
            }

            // Fallback for other error formats
            return {
                message: data?.message || `Request failed with status ${status}`,
                status,
                code: data?.code || `HTTP_${status}`
            }
        } else if (error.request) {
            // Request was made but no response received
            return {
                message: 'No response from server. Please check your connection.',
                code: 'NETWORK_ERROR'
            }
        } else {
            // Something else happened
            return {
                message: error.message || 'An unexpected error occurred',
                code: 'UNKNOWN_ERROR'
            }
        }
    }

    static getErrorMessage(error: ApiError): string {
        // If we have a server message, use it directly
        if (error.message && error.message !== 'Request failed with status ' + error.status) {
            return error.message
        }

        // Fallback to status-based messages
        switch (error.status) {
            case 400:
                return 'Invalid request. Please check your input.'
            case 401:
                return 'Authentication failed. Please log in again.'
            case 403:
                return 'Access denied. You don\'t have permission for this action.'
            case 404:
                return 'Resource not found.'
            case 405:
                return 'Method not allowed.'
            case 409:
                return 'Resource already exists.'
            case 422:
                return 'Validation error. Please check your input.'
            case 500:
                return 'Server error. Please try again later.'
            default:
                return error.message || 'An unexpected error occurred'
        }
    }

    static getFieldErrors(error: ApiError): Record<string, string> | null {
        return error.fieldErrors || null
    }

    static isNetworkError(error: ApiError): boolean {
        return error.code === 'NETWORK_ERROR'
    }

    static isAuthError(error: ApiError): boolean {
        return error.status === 401 || error.status === 403
    }

    static isValidationError(error: ApiError): boolean {
        return error.status === 400 || error.status === 422
    }
}
