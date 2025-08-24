import { ref } from 'vue'

export function useCsvUpload() {
  // Reactive state
  const selectedFile = ref<File | null>(null)
  const isDragOver = ref(false)
  const isUploading = ref(false)
  const isDownloading = ref(false)
  const uploadProgress = ref(0)
  const uploadResult = ref<any>(null)
  /**
   * Validate CSV file
   */
  const validateCsvFile = (file: File): { isValid: boolean; error?: string } => {
    // Check file type
    if (!file.name.toLowerCase().endsWith('.csv')) {
      return { isValid: false, error: 'Please select a valid CSV file' }
    }

    // Check file size (10MB limit)
    if (file.size > 10 * 1024 * 1024) {
      return { isValid: false, error: 'File size must be less than 10MB' }
    }

    return { isValid: true }
  }

  /**
   * Format file size for display
   */
  const formatFileSize = (bytes: number): string => {
    if (bytes === 0) return '0 Bytes'
    const k = 1024
    const sizes = ['Bytes', 'KB', 'MB', 'GB']
    const i = Math.floor(Math.log(bytes) / Math.log(k))
    return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i]
  }

  /**
   * Create FormData for file upload
   */
  const createUploadFormData = (file: File, inventoryId: string, userId: string): FormData => {
    const formData = new FormData()
    formData.append('file', file)
    formData.append('inventoryId', inventoryId)
    formData.append('userId', userId)
    return formData
  }

  /**
   * Simulate upload progress
   */
  const simulateProgress = (callback: (progress: number) => void, duration: number = 2000): void => {
    let progress = 0
    const interval = setInterval(() => {
      if (progress < 90) {
        progress += 10
        callback(progress)
      } else {
        clearInterval(interval)
      }
    }, duration / 9)
  }

  /**
   * Get result border class based on upload results
   */
  const getResultBorderClass = (uploadResult: any): string => {
    if (!uploadResult) return ''

    if (uploadResult.failedItems === 0) {
      return 'border-border bg-muted'
    } else if (uploadResult.successfulItems === 0) {
      return 'border-border bg-muted'
    } else {
      return 'border-border bg-muted'
    }
  }

  /**
   * Handle file selection and validation
   */
  const handleFileSelect = (event: Event, onSuccess?: () => void, onError?: (error: string) => void): void => {
    const target = event.target as HTMLInputElement
    if (target.files && target.files[0]) {
      const file = target.files[0]
      const validation = validateCsvFile(file)
      
      if (validation.isValid) {
        selectedFile.value = file
        onSuccess?.()
      } else {
        selectedFile.value = null
        onError?.(validation.error || 'Invalid file')
      }
    }
  }

  /**
   * Handle file drop and validation
   */
  const handleFileDrop = (event: DragEvent, onSuccess?: () => void, onError?: (error: string) => void): void => {
    isDragOver.value = false
    if (event.dataTransfer?.files && event.dataTransfer.files[0]) {
      const file = event.dataTransfer.files[0]
      const validation = validateCsvFile(file)
      
      if (validation.isValid) {
        selectedFile.value = file
        onSuccess?.()
      } else {
        selectedFile.value = null
        onError?.(validation.error || 'Invalid file')
      }
    }
  }

  /**
   * Reset upload state
   */
  const resetUploadState = () => {
    selectedFile.value = null
    uploadResult.value = null
    uploadProgress.value = 0
  }

  /**
   * Trigger file input click
   */
  const triggerFileInput = (fileInput: any) => {
    fileInput.value?.click()
  }

  /**
   * Set drag over state
   */
  const setDragOver = (value: boolean) => {
    isDragOver.value = value
  }

  /**
   * Download CSV template from blob
   */
  const downloadTemplateFromBlob = (blob: Blob, filename: string = 'inventory_items_template.csv'): void => {
    const url = window.URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = filename
    link.style.visibility = 'hidden'
    document.body.appendChild(link)
    link.click()
    document.body.removeChild(link)
    window.URL.revokeObjectURL(url)
  }

  /**
   * Validate upload prerequisites
   */
  const validateUploadPrerequisites = (file: File | null, userId: string | undefined): { isValid: boolean; error?: string } => {
    if (!file) {
      return { isValid: false, error: 'Please select a file' }
    }
    
    if (!userId) {
      return { isValid: false, error: 'Please ensure you are authenticated' }
    }

    return { isValid: true }
  }

  /**
   * Process upload result and show appropriate messages
   */
  const processUploadResult = (result: any, onSuccess?: (message: string) => void, onError?: (message: string) => void): any => {
    uploadProgress.value = 100

    if (result.success && result.data) {
      uploadResult.value = result.data

      if (result.data.successfulItems > 0) {
        onSuccess?.(`Successfully uploaded ${result.data.successfulItems} items`)
      }

      if (result.data.failedItems > 0) {
        onError?.(`${result.data.failedItems} items failed to upload`)
      }

      return result.data
    } else {
      onError?.(result.error || 'Upload failed')
      return null
    }
  }

  /**
   * Handle template download
   */
  const handleTemplateDownload = async (downloadService: any, onSuccess?: () => void, onError?: (error: string) => void) => {
    try {
      isDownloading.value = true
      const result = await downloadService()

      if (result.success && result.data) {
        downloadTemplateFromBlob(result.data)
        onSuccess?.()
      } else {
        onError?.(result.error || 'Failed to download template')
      }
    } catch (error) {
      console.error('Download error:', error)
      onError?.('Failed to download template')
    } finally {
      isDownloading.value = false
    }
  }

  /**
   * Handle complete upload process
   */
  const handleUpload = async (
    uploadService: any,
    inventoryId: string,
    userId: string,
    onSuccess?: (message: string) => void,
    onError?: (message: string) => void,
    onComplete?: () => void
  ) => {
    const validation = validateUploadPrerequisites(selectedFile.value, userId)
    
    if (!validation.isValid) {
      onError?.(validation.error || 'Validation failed')
      return
    }

    try {
      isUploading.value = true
      uploadProgress.value = 0

      // Simulate progress
      simulateProgress((progress) => {
        uploadProgress.value = progress
      }, 2000)

      const result = await uploadService(selectedFile.value!, inventoryId, userId)
      
      const processedResult = processUploadResult(result, onSuccess, onError)
      
      if (processedResult && onComplete) {
        onComplete()
      }
    } catch (error) {
      console.error('Upload error:', error)
      onError?.('Failed to upload file')
    } finally {
      isUploading.value = false
    }
  }

  /**
   * Handle upload with toast notifications (convenience wrapper)
   */
  const handleUploadWithToast = async (
    uploadService: any,
    inventoryId: string,
    userId: string,
    toast: any,
    onComplete?: () => void
  ) => {
    await handleUpload(
      uploadService,
      inventoryId,
      userId,
      (message) => toast.success(message),
      (message) => toast.error(message),
      onComplete
    )
  }

  /**
   * Handle template download with toast notifications (convenience wrapper)
   */
  const handleTemplateDownloadWithToast = async (downloadService: any, toast: any) => {
    await handleTemplateDownload(
      downloadService,
      () => toast.success("Template downloaded successfully"),
      (error) => toast.error(error)
    )
  }

  /**
   * Handle file selection with toast notifications (convenience wrapper)
   */
  const handleFileSelectWithToast = (event: Event, toast: any) => {
    handleFileSelect(
      event,
      () => toast.success("File selected successfully"),
      (error) => toast.error(error)
    )
  }

  /**
   * Handle file drop with toast notifications (convenience wrapper)
   */
  const handleFileDropWithToast = (event: DragEvent, toast: any) => {
    handleFileDrop(
      event,
      () => toast.success("File selected successfully"),
      (error) => toast.error(error)
    )
  }

  return {
    // State
    selectedFile,
    isDragOver,
    isUploading,
    isDownloading,
    uploadProgress,
    uploadResult,
    
    // Validation
    validateCsvFile,
    validateUploadPrerequisites,
    
    // File handling
    handleFileSelect,
    handleFileDrop,
    triggerFileInput,
    setDragOver,
    
    // State management
    resetUploadState,
    
    // Upload/Download
    handleUpload,
    handleTemplateDownload,
    
    // Convenience methods with toast
    handleUploadWithToast,
    handleTemplateDownloadWithToast,
    handleFileSelectWithToast,
    handleFileDropWithToast,
    
    // Utilities
    formatFileSize,
    createUploadFormData,
    simulateProgress,
    getResultBorderClass,
    downloadTemplateFromBlob,
    processUploadResult
  }
}
