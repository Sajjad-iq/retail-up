import { ref } from 'vue'

export function useInventoryItemDialogs() {
  // Dialog state
  const selectedItem = ref<any>(null)
  const showDialog = ref(false)
  const showDetailsDialog = ref(false)
  const showDeleteDialog = ref(false)
  const showImportDialog = ref(false)
  const dialogMode = ref<'create' | 'edit'>('create')

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

  return {
    // State
    selectedItem,
    showDialog,
    showDetailsDialog,
    showDeleteDialog,
    showImportDialog,
    dialogMode,

    // Actions
    openCreateDialog,
    editItem,
    viewItemDetails,
    deleteItem,
    openImportDialog,
    closeAllDialogs,
    handleDialogSuccess,
    handleImportSuccess,
    handleDeleteConfirm
  }
}
