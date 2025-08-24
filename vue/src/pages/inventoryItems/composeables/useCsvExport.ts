import type { InventoryItem } from '@/types/global'

export function useCsvExport() {
  /**
   * Convert inventory items to CSV format
   */
  const convertToCsv = (items: InventoryItem[]): string => {
    // Define CSV headers (matching backend template structure)
    const headers = [
      'Name', 'Description', 'SKU', 'Product Code', 'Barcode',
      'Category', 'Brand', 'Unit', 'Weight', 'Dimensions',
      'Color', 'Size', 'Current Stock', 'Minimum Stock', 'Maximum Stock',
      'Cost Price Amount', 'Cost Price Currency', 'Selling Price Amount',
      'Selling Price Currency', 'Discount Price', 'Discount Start Date',
      'Discount End Date', 'Supplier Name', 'Is Perishable', 'Expiry Date'
    ]

    // Convert items to CSV rows
    const csvRows = items.map(item => [
      item.name || '',
      item.description || '',
      item.sku || '',
      item.productCode || '',
      item.barcode || '',
      item.category || '',
      item.brand || '',
      item.unit || '',
      item.weight || '',
      item.dimensions || '',
      item.color || '',
      item.size || '',
      item.currentStock || 0,
      item.minimumStock || '',
      item.maximumStock || '',
      item.costPrice?.amount || '',
      item.costPrice?.currency || '',
      item.sellingPrice?.amount || '',
      item.sellingPrice?.currency || '',
      item.discountPrice || '',
      item.discountStartDate || '',
      item.discountEndDate || '',
      item.supplierName || '',
      item.isPerishable || false,
      item.expiryDate || ''
    ])

    // Create CSV content
    const csvContent = [
      headers.join(','),
      ...csvRows.map(row => row.map(cell => `"${cell}"`).join(','))
    ].join('\n')

    return csvContent
  }

  /**
   * Download CSV file from content
   */
  const downloadCsv = (csvContent: string, filename: string): void => {
    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' })
    const link = document.createElement('a')
    const url = URL.createObjectURL(blob)
    link.setAttribute('href', url)
    link.setAttribute('download', filename)
    link.style.visibility = 'hidden'
    document.body.appendChild(link)
    link.click()
    document.body.removeChild(link)
    URL.revokeObjectURL(url)
  }

  /**
   * Get human-readable filter description for filename
   */
  const getFilterDescription = (filters: any): string => {
    const parts = []
    
    if (filters.searchTerm) parts.push(`search_${filters.searchTerm}`)
    if (filters.category) parts.push(`category_${filters.category}`)
    if (filters.brand) parts.push(`brand_${filters.brand}`)
    if (filters.isActive !== undefined) parts.push(`active_${filters.isActive}`)
    
    return parts.length > 0 ? parts.join('_') : 'all'
  }

  /**
   * Generate filename with filter information
   */
  const generateFilename = (filters: any, baseName: string = 'inventory_items'): string => {
    const filterInfo = getFilterDescription(filters)
    const date = new Date().toISOString().split('T')[0]
    return `${baseName}_${filterInfo}_${date}.csv`
  }

  return {
    convertToCsv,
    downloadCsv,
    getFilterDescription,
    generateFilename
  }
}
