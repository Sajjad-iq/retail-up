<template>
  <Dialog :open="open" @update:open="$emit('update:open', $event)">
    <DialogContent class="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle>Import Inventory Items from Excel</DialogTitle>
        <DialogDescription>
          Upload a CSV file to create multiple inventory items at once. Download the template first
          to ensure correct format.
        </DialogDescription>
      </DialogHeader>

      <div class="space-y-6">
        <!-- Template Download Section -->
        <div class="bg-muted/50 p-4 rounded-lg">
          <div class="flex items-center justify-between">
            <div>
              <h3 class="font-medium text-sm">CSV Template</h3>
              <p class="text-sm text-muted-foreground mt-1">
                Download the template to see the required column structure
              </p>
            </div>
            <Button variant="outline" size="sm" @click="downloadTemplate" :disabled="isDownloading">
              <Download class="w-4 h-4 mr-2" />
              {{ isDownloading ? "Downloading..." : "Download Template" }}
            </Button>
          </div>
        </div>

        <!-- File Upload Section -->
        <div class="space-y-4">
          <div>
            <Label for="file-upload" class="text-sm font-medium">Select CSV File</Label>
            <p class="text-sm text-muted-foreground mt-1">
              Only CSV files are supported. Maximum file size: 10MB
            </p>
          </div>

          <!-- File Drop Zone -->
          <div
            class="border-2 border-dashed border-border rounded-lg p-6 text-center hover:border-primary/50 transition-colors"
            :class="{ 'border-primary bg-primary/5': isDragOver }"
            @drop.prevent="handleFileDrop"
            @dragover.prevent="setDragOver(true)"
            @dragleave.prevent="setDragOver(false)"
            @click="triggerFileInput"
          >
            <div v-if="!selectedFile" class="space-y-2">
              <Upload class="w-8 h-8 mx-auto text-muted-foreground" />
              <div>
                <p class="text-sm font-medium">Drop your CSV file here</p>
                <p class="text-xs text-muted-foreground">or click to browse</p>
              </div>
            </div>

            <div v-else class="space-y-2">
              <FileText class="w-8 h-8 mx-auto text-primary" />
              <div>
                <p class="text-sm font-medium">{{ selectedFile.name }}</p>
                <p class="text-xs text-muted-foreground">
                  {{ formatFileSize(selectedFile.size) }}
                </p>
              </div>
              <Button variant="outline" size="sm" @click="removeFile"> Remove File </Button>
            </div>
          </div>

          <!-- Hidden file input -->
          <input
            ref="fileInput"
            type="file"
            accept=".csv"
            class="hidden"
            @change="handleFileSelect"
          />
        </div>

        <!-- Upload Progress -->
        <div v-if="isUploading" class="space-y-2">
          <div class="flex items-center justify-between text-sm">
            <span>Uploading...</span>
            <span>{{ uploadProgress }}%</span>
          </div>
          <div class="w-full bg-muted rounded-full h-2">
            <div
              class="bg-primary h-2 rounded-full transition-all duration-300 ease-out"
              :style="{ width: uploadProgress + '%' }"
            ></div>
          </div>
        </div>

        <!-- Upload Results -->
        <div v-if="uploadResult" class="space-y-3">
          <div class="p-4 rounded-lg border" :class="getResultBorderClass()">
            <div class="flex items-center space-x-2">
              <CheckCircle v-if="uploadResult.successfulItems > 0" class="w-5 h-5 text-chart-1" />
              <AlertCircle v-if="uploadResult.failedItems > 0" class="w-5 h-5 text-destructive" />
              <h4 class="font-medium">Upload Complete</h4>
            </div>

            <div class="mt-2 space-y-1 text-sm">
              <p>Total rows processed: {{ uploadResult.totalRows }}</p>
              <p class="text-chart-1">
                Successfully created: {{ uploadResult.successfulItems }} items
              </p>
              <p v-if="uploadResult.failedItems > 0" class="text-destructive">
                Failed: {{ uploadResult.failedItems }} items
              </p>
            </div>

            <!-- Error Details -->
            <div v-if="uploadResult.errors && uploadResult.errors.length > 0" class="mt-3">
              <details class="text-sm">
                <summary class="cursor-pointer text-destructive font-medium">
                  View Error Details
                </summary>
                <ul class="mt-2 space-y-1 text-xs text-destructive">
                  <li v-for="(error, index) in uploadResult.errors" :key="index">
                    {{ error }}
                  </li>
                </ul>
              </details>
            </div>
          </div>
        </div>

        <!-- Form Actions -->
        <div class="flex justify-end space-x-2 pt-4">
          <Button type="button" variant="outline" @click="$emit('update:open', false)">
            Cancel
          </Button>
          <Button type="button" @click="handleUpload" :disabled="!selectedFile || isUploading">
            <span v-if="isUploading" class="flex items-center gap-2">
              <div
                class="animate-spin rounded-full h-4 w-4 border-b-2 border-primary-foreground"
              ></div>
              Uploading...
            </span>
            <span v-else>Upload Items</span>
          </Button>
        </div>
      </div>
    </DialogContent>
  </Dialog>
</template>

<script setup lang="ts">
import { useInventoryItemDialogs } from "@/composables/useInventoryItemDialogs";

// UI Components
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";

// Icons
import { Upload, Download, FileText, CheckCircle, AlertCircle } from "lucide-vue-next";

interface Props {
  open: boolean;
  inventoryId: string;
}

const props = defineProps<Props>();

// Use the composable for all dialog and upload functionality
const {
  fileInput,
  selectedFile,
  isDragOver,
  isUploading,
  isDownloading,
  uploadProgress,
  uploadResult,
  triggerFileInput,
  handleFileSelect,
  handleFileDrop,
  removeFile,
  formatFileSize,
  downloadTemplate,
  getResultBorderClass,
  setDragOver,
  handleUpload: handleUploadFromComposable,
} = useInventoryItemDialogs();

// Handle upload with inventory ID
const handleUpload = async () => {
  await handleUploadFromComposable(props.inventoryId);
};
</script>
