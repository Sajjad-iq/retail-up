export interface ApiError {
    message: string
    status?: number
    code?: string
}

export class ErrorHandler {
    static handleApiError(error: any): ApiError {
        if (error.response) {
            // Server responded with error status
            const { status, data } = error.response
            return {
                message: data?.message || `Request failed with status ${status}`,
                status,
                code: data?.code
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
        switch (error.status) {
            case 400:
                return 'Invalid request. Please check your input.'
            case 401:
                return 'Authentication failed. Please log in again.'
            case 403:
                return 'Access denied. You don\'t have permission for this action.'
            case 404:
                return 'Resource not found.'
            case 422:
                return 'Validation error. Please check your input.'
            case 500:
                return 'Server error. Please try again later.'
            default:
                return error.message || 'An unexpected error occurred'
        }
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
