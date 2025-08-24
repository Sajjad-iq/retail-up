import { ref } from 'vue'
import { toast } from 'vue-sonner'
import { useAuthStore } from '@/stores/auth'
import { excelUploadService } from '@/services/excelUploadService'
import { useCsvUpload } from '@/pages/inventoryItems/composeables/useCsvUpload'

export function useInventoryItemDialogs() {
  // Dialog state
  const selectedItem = ref<any>(null)
  const showDialog = ref(false)
  const showDetailsDialog = ref(false)
  const showDeleteDialog = ref(false)
  const showImportDialog = ref(false)
  const dialogMode = ref<'create' | 'edit'>('create')

  // Excel upload state and composable
  const authStore = useAuthStore()
  const csvUpload = useCsvUpload()
  const fileInput = ref<HTMLInputElement>()

  // Extract state from CSV upload composable
  const { selectedFile, isDragOver, isUploading, isDownloading, uploadProgress, uploadResult } = csvUpload

  // Dialog actions
  const openCreateDialog = () => {
    selectedItem.value = null
    dialogMode.value = 'create'
    showDialog.value = true
  }

  const editItem = (item: any) => {
    selectedItem.value = item
    dialogMode.value = 'edit'
    showDialog.value = true
  }

  const viewItemDetails = (item: any) => {
    selectedItem.value = item
    showDetailsDialog.value = true
  }

  const deleteItem = (item: any) => {
    selectedItem.value = item
    showDeleteDialog.value = true
  }

  const openImportDialog = () => {
    showImportDialog.value = true
  }

  const closeAllDialogs = () => {
    showDialog.value = false
    showDetailsDialog.value = false
    showDeleteDialog.value = false
    showImportDialog.value = false
    selectedItem.value = null
  }

  const handleDialogSuccess = () => {
    showDialog.value = false
  }

  const handleImportSuccess = () => {
    showImportDialog.value = false
  }

  const handleDeleteConfirm = () => {
    showDeleteDialog.value = false
    selectedItem.value = null
  }

  // Excel upload methods (simplified using CSV composable convenience methods)
  const triggerFileInput = () => {
    csvUpload.triggerFileInput(fileInput)
  }

  const handleFileSelect = (event: Event) => {
    csvUpload.handleFileSelectWithToast(event, toast)
  }

  const handleFileDrop = (event: DragEvent) => {
    csvUpload.handleFileDropWithToast(event, toast)
  }

  const removeFile = () => {
    csvUpload.resetUploadState()
  }

  const formatFileSize = (bytes: number): string => {
    return csvUpload.formatFileSize(bytes)
  }

  const downloadTemplate = async () => {
    await csvUpload.handleTemplateDownloadWithToast(
      () => excelUploadService.downloadTemplate(),
      toast
    )
  }

  const handleUpload = async (inventoryId: string) => {
    try {
      await csvUpload.handleUploadWithToast(
        (file: File, inventoryId: string, userId: string) =>
          excelUploadService.uploadCsvFile(file, inventoryId, userId),
        inventoryId,
        authStore.user?.id || "",
        toast
      )
    } catch (error) {
      toast.error("Upload failed: " + (error instanceof Error ? error.message : "Unknown error"))
    }
  }

  const getResultBorderClass = () => {
    return csvUpload.getResultBorderClass(uploadResult.value)
  }

  const setDragOver = (value: boolean) => {
    csvUpload.setDragOver(value)
  }

  return {
    // State
    selectedItem,
    showDialog,
    showDetailsDialog,
    showDeleteDialog,
    showImportDialog,
    dialogMode,

    // Excel upload state
    fileInput,
    selectedFile,
    isDragOver,
    isUploading,
    isDownloading,
    uploadProgress,
    uploadResult,

    // Actions
    openCreateDialog,
    editItem,
    viewItemDetails,
    deleteItem,
    openImportDialog,
    closeAllDialogs,
    handleDialogSuccess,
    handleImportSuccess,
    handleDeleteConfirm,

    // Excel upload methods
    triggerFileInput,
    handleFileSelect,
    handleFileDrop,
    removeFile,
    formatFileSize,
    downloadTemplate,
    handleUpload,
    getResultBorderClass,
    setDragOver
  }
}
