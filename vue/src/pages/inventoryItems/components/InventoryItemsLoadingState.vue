<template>
  <div class="bg-card rounded-lg border p-8">
    <div class="text-center">
      <div class="mx-auto h-12 w-12 text-muted-foreground">
        <svg class="animate-spin h-12 w-12" fill="none" viewBox="0 0 24 24">
          <circle
            class="opacity-25"
            cx="12"
            cy="12"
            r="10"
            stroke="currentColor"
            stroke-width="4"
          ></circle>
          <path
            class="opacity-75"
            fill="currentColor"
            d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
          ></path>
        </svg>
      </div>
      <h3 class="mt-2 text-sm font-medium text-foreground">Loading inventory items</h3>
      <p class="mt-1 text-sm text-muted-foreground">
        Please wait while we fetch your inventory data.
      </p>
    </div>
  </div>
</template>
