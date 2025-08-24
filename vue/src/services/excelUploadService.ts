import { httpService } from '../config/http'
import { ErrorHandler } from '@/utils/errorHandler'
import type { ApiResponse } from '.'

// Backend ExcelUploadResponse structure
export interface ExcelUploadResponse {
  totalRows: number
  successfulItems: number
  failedItems: number
  createdItems: any[]
  errors: string[]
}

class ExcelUploadService {
  private axios = httpService.getAxiosInstance()

  /**
   * Upload CSV file to create inventory items in bulk
   */
  async uploadCsvFile(file: File, inventoryId: string, userId: string): Promise<ApiResponse<ExcelUploadResponse>> {
    try {
      const formData = new FormData()
      formData.append('file', file)
      formData.append('inventoryId', inventoryId)
      formData.append('userId', userId)

      const response = await this.axios.post<ExcelUploadResponse>('/v1/inventory/csv-upload/upload', formData, {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      })
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }

  /**
   * Download CSV template for inventory items from backend
   */
  async downloadTemplate(): Promise<ApiResponse<Blob> | ApiResponse<null>> {
    try {
      const response = await this.axios.get('/v1/inventory/csv-upload/template', {
        responseType: 'blob',
        headers: {
          'Accept': 'text/csv'
        }
      })
      
      return { success: true, data: response.data }
    } catch (error: any) {
      const apiError = ErrorHandler.handleApiError(error)
      return { success: false, error: ErrorHandler.getErrorMessage(apiError) }
    }
  }
}

// Export singleton instance
export const excelUploadService = new ExcelUploadService()
