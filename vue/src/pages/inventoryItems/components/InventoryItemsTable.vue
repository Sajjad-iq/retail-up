<template>
  <div class="w-full">
    <!-- Table Header -->
    <div class="px-6 py-4 border-b border-border bg-card rounded-lg border">
      <h3 class="text-lg font-medium text-foreground">Inventory Items</h3>
    </div>

    <!-- Column Visibility Controls -->
    <div class="flex gap-2 items-center py-4">
      <DropdownMenu>
        <DropdownMenuTrigger as-child>
          <Button variant="outline"> Columns <ChevronDown class="ml-2 h-4 w-4" /> </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end">
          <DropdownMenuCheckboxItem
            v-for="column in table.getAllColumns().filter((column) => column.getCanHide())"
            :key="column.id"
            class="capitalize"
            :model-value="column.getIsVisible()"
            @update:model-value="
              (value) => {
                column.toggleVisibility(!!value);
              }
            "
          >
            {{ column.id }}
          </DropdownMenuCheckboxItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </div>

    <!-- Table -->
    <div class="rounded-md border bg-card">
      <Table>
        <TableHeader>
          <TableRow v-for="headerGroup in table.getHeaderGroups()" :key="headerGroup.id">
            <TableHead v-for="header in headerGroup.headers" :key="header.id">
              <FlexRender
                v-if="!header.isPlaceholder"
                :render="header.column.columnDef.header"
                :props="header.getContext()"
              />
            </TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          <template v-if="table.getRowModel().rows?.length">
            <template v-for="row in table.getRowModel().rows" :key="row.id">
              <TableRow :data-state="row.getIsSelected() && 'selected'" class="hover:bg-muted">
                <TableCell v-for="cell in row.getVisibleCells()" :key="cell.id">
                  <FlexRender :render="cell.column.columnDef.cell" :props="cell.getContext()" />
                </TableCell>
              </TableRow>
            </template>
          </template>

          <TableRow v-else>
            <TableCell :colspan="columns.length" class="h-24 text-center"> No results. </TableCell>
          </TableRow>
        </TableBody>
      </Table>
    </div>

    <!-- Pagination -->
    <div class="flex items-center justify-end space-x-2 py-4">
      <div class="flex-1 text-sm text-muted-foreground">
        {{ table.getFilteredSelectedRowModel().rows.length }} of
        {{ table.getFilteredRowModel().rows.length }} row(s) selected.
      </div>
      <div class="space-x-2">
        <Button
          variant="outline"
          size="sm"
          :disabled="!table.getCanPreviousPage()"
          @click="table.previousPage()"
        >
          Previous
        </Button>
        <Button
          variant="outline"
          size="sm"
          :disabled="!table.getCanNextPage()"
          @click="table.nextPage()"
        >
          Next
        </Button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import type {
  ColumnDef,
  ColumnFiltersState,
  SortingState,
  VisibilityState,
} from "@tanstack/vue-table";
import {
  FlexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useVueTable,
} from "@tanstack/vue-table";
import { ArrowUpDown, ChevronDown } from "lucide-vue-next";

import { h, ref } from "vue";

import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { CubeIcon, EyeIcon, PencilIcon, TrashIcon } from "@heroicons/vue/24/outline";
import type { InventoryItem } from "@/types/global";

interface Props {
  items: InventoryItem[];
  pagination: {
    currentPage: number;
    totalPages: number;
    totalElements: number;
    pageSize: number;
    hasNext: boolean;
    hasPrevious: boolean;
  } | null;
}

interface Emits {
  (e: "edit", item: InventoryItem): void;
  (e: "delete", item: InventoryItem): void;
  (e: "view", item: InventoryItem): void;
  (e: "pageChange", page: number): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

// Define columns for the data table
const columns: ColumnDef<InventoryItem>[] = [
  {
    id: "select",
    header: ({ table }) =>
      h(Checkbox, {
        modelValue:
          table.getIsAllPageRowsSelected() ||
          (table.getIsSomePageRowsSelected() && "indeterminate"),
        "onUpdate:modelValue": (value) => table.toggleAllPageRowsSelected(!!value),
        ariaLabel: "Select all",
      }),
    cell: ({ row }) =>
      h(Checkbox, {
        modelValue: row.getIsSelected(),
        "onUpdate:modelValue": (value) => row.toggleSelected(!!value),
        ariaLabel: "Select row",
      }),
    enableSorting: false,
    enableHiding: false,
  },
  {
    accessorKey: "name",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Item", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "flex items-center" }, [
        h("div", { class: "flex-shrink-0 h-10 w-10" }, [
          h("div", { class: "h-10 w-10 rounded-lg bg-muted flex items-center justify-center" }, [
            h(CubeIcon, { class: "h-6 w-6 text-muted-foreground" }),
          ]),
        ]),
        h("div", { class: "ml-4" }, [
          h("div", { class: "text-sm font-medium text-foreground" }, item.name),
          item.description
            ? h(
                "div",
                { class: "text-sm text-muted-foreground truncate max-w-xs" },
                item.description
              )
            : null,
        ]),
      ]);
    },
  },
  {
    accessorKey: "sku",
    header: "SKU/Code",
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "space-y-1" }, [
        item.sku
          ? h("div", { class: "text-sm text-foreground" }, [
              h("span", { class: "font-medium" }, "SKU: "),
              item.sku,
            ])
          : null,
        item.productCode
          ? h("div", { class: "text-sm text-muted-foreground" }, [
              h("span", { class: "font-medium" }, "Code: "),
              item.productCode,
            ])
          : null,
      ]);
    },
  },
  {
    accessorKey: "barcode",
    header: "Barcode",
    cell: ({ row }) => {
      const item = row.original;
      return item.barcode
        ? h("div", { class: "text-sm text-foreground font-mono" }, item.barcode)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "category",
    header: "Category",
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "space-y-1" }, [
        item.category ? h("div", { class: "text-sm text-foreground" }, item.category) : null,
        item.brand ? h("div", { class: "text-sm text-muted-foreground" }, item.brand) : null,
      ]);
    },
  },
  {
    accessorKey: "unit",
    header: "Unit",
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "text-sm text-foreground" }, item.unit);
    },
  },
  {
    accessorKey: "currentStock",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Stock", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "space-y-1" }, [
        h("div", { class: "text-sm text-foreground" }, [
          h("span", { class: "font-medium" }, item.currentStock),
          h("span", { class: "text-muted-foreground" }, ` ${item.unit}`),
        ]),
        item.minimumStock !== undefined
          ? h("div", { class: "text-xs text-muted-foreground" }, `Min: ${item.minimumStock}`)
          : null,
        item.maximumStock !== undefined
          ? h("div", { class: "text-xs text-muted-foreground" }, `Max: ${item.maximumStock}`)
          : null,
      ]);
    },
  },
  {
    accessorKey: "costPrice",
    header: () => h("div", { class: "text-right" }, "Cost Price"),
    cell: ({ row }) => {
      const item = row.original;
      return item.costPrice
        ? h("div", { class: "text-sm text-foreground text-right" }, `$${item.costPrice.amount}`)
        : h("div", { class: "text-sm text-muted-foreground text-right" }, "—");
    },
  },
  {
    accessorKey: "sellingPrice",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Selling Price", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "space-y-1 text-right" }, [
        h("div", { class: "text-sm text-foreground" }, [
          h("span", { class: "font-medium" }, `$${item.sellingPrice.amount}`),
          h("span", { class: "text-xs text-muted-foreground" }, ` ${item.sellingPrice.currency}`),
        ]),
        item.discountPrice
          ? h("div", { class: "text-xs text-red-600" }, `Disc: $${item.discountPrice}`)
          : null,
      ]);
    },
  },
  {
    accessorKey: "totalSold",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Total Sold", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "text-sm text-foreground" }, item.totalSold);
    },
  },
  {
    accessorKey: "totalRevenue",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Revenue", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "text-sm text-foreground" }, `$${item.totalRevenue}`);
    },
  },
  {
    accessorKey: "isActive",
    header: "Status",
    cell: ({ row }) => {
      const item = row.original;
      return h(
        Badge,
        { variant: item.isActive ? "default" : "secondary" },
        item.isActive ? "Active" : "Inactive"
      );
    },
  },
  {
    accessorKey: "isPerishable",
    header: "Perishable",
    cell: ({ row }) => {
      const item = row.original;
      return h(
        Badge,
        { variant: item.isPerishable ? "destructive" : "secondary" },
        item.isPerishable ? "Yes" : "No"
      );
    },
  },
  {
    accessorKey: "expiryDate",
    header: "Expiry",
    cell: ({ row }) => {
      const item = row.original;
      return item.expiryDate
        ? h(
            "div",
            { class: "text-sm text-foreground" },
            new Date(item.expiryDate).toLocaleDateString()
          )
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "lastSoldDate",
    header: "Last Sold",
    cell: ({ row }) => {
      const item = row.original;
      return item.lastSoldDate
        ? h(
            "div",
            { class: "text-sm text-foreground" },
            new Date(item.lastSoldDate).toLocaleDateString()
          )
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "weight",
    header: "Weight",
    cell: ({ row }) => {
      const item = row.original;
      return item.weight
        ? h("div", { class: "text-sm text-foreground" }, `${item.weight} kg`)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "dimensions",
    header: "Dimensions",
    cell: ({ row }) => {
      const item = row.original;
      return item.dimensions
        ? h("div", { class: "text-sm text-foreground" }, item.dimensions)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "color",
    header: "Color",
    cell: ({ row }) => {
      const item = row.original;
      return item.color
        ? h("div", { class: "text-sm text-foreground" }, item.color)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "size",
    header: "Size",
    cell: ({ row }) => {
      const item = row.original;
      return item.size
        ? h("div", { class: "text-sm text-foreground" }, item.size)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "discountStartDate",
    header: "Discount Start",
    cell: ({ row }) => {
      const item = row.original;
      return item.discountStartDate
        ? h(
            "div",
            { class: "text-sm text-foreground" },
            new Date(item.discountStartDate).toLocaleDateString()
          )
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "discountEndDate",
    header: "Discount End",
    cell: ({ row }) => {
      const item = row.original;
      return item.discountEndDate
        ? h(
            "div",
            { class: "text-sm text-foreground" },
            new Date(item.discountEndDate).toLocaleDateString()
          )
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "supplierName",
    header: "Supplier",
    cell: ({ row }) => {
      const item = row.original;
      return item.supplierName
        ? h("div", { class: "text-sm text-foreground" }, item.supplierName)
        : h("div", { class: "text-sm text-muted-foreground" }, "—");
    },
  },
  {
    accessorKey: "inventoryId",
    header: "Inventory ID",
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "text-sm text-foreground font-mono" }, item.inventoryId);
    },
  },
  {
    accessorKey: "createdAt",
    header: ({ column }) => {
      return h(
        Button,
        {
          variant: "ghost",
          onClick: () => column.toggleSorting(column.getIsSorted() === "asc"),
        },
        () => ["Created", h(ArrowUpDown, { class: "ml-2 h-4 w-4" })]
      );
    },
    cell: ({ row }) => {
      const item = row.original;
      return h(
        "div",
        { class: "text-sm text-foreground" },
        new Date(item.createdAt).toLocaleDateString()
      );
    },
  },
  {
    id: "actions",
    header: "Actions",
    enableHiding: false,
    cell: ({ row }) => {
      const item = row.original;
      return h("div", { class: "flex items-center space-x-2" }, [
        h(
          Button,
          {
            onClick: () => emit("view", item),
            variant: "ghost",
            size: "sm",
            class: "h-8 w-8 p-0",
          },
          () => [h(EyeIcon, { class: "h-4 w-4" })]
        ),
        h(
          Button,
          {
            onClick: () => emit("edit", item),
            variant: "ghost",
            size: "sm",
            class: "h-8 w-8 p-0",
          },
          () => [h(PencilIcon, { class: "h-4 w-4" })]
        ),
        h(
          Button,
          {
            onClick: () => emit("delete", item),
            variant: "ghost",
            size: "sm",
            class: "h-8 w-8 p-0 text-red-600 hover:text-red-700",
          },
          () => [h(TrashIcon, { class: "h-4 w-4" })]
        ),
      ]);
    },
  },
];

const sorting = ref<SortingState>([]);
const columnFilters = ref<ColumnFiltersState>([]);
const columnVisibility = ref<VisibilityState>({
  // Essential columns - visible by default
  select: true,
  name: true,
  sku: true,
  currentStock: true,
  sellingPrice: true,
  isActive: true,
  actions: true,

  // Less important columns - hidden by default
  barcode: false,
  category: false,
  unit: false,
  costPrice: false,
  totalSold: false,
  totalRevenue: false,
  isPerishable: false,
  expiryDate: false,
  lastSoldDate: false,
  weight: false,
  dimensions: false,
  color: false,
  size: false,
  discountStartDate: false,
  discountEndDate: false,
  supplierName: false,
  inventoryId: false,
  createdAt: false,
});
const rowSelection = ref({});

const table = useVueTable<InventoryItem>({
  data: props.items,
  columns,
  getCoreRowModel: getCoreRowModel(),
  getPaginationRowModel: getPaginationRowModel(),
  getSortedRowModel: getSortedRowModel(),
  getFilteredRowModel: getFilteredRowModel(),
  onSortingChange: (updaterOrValue) => {
    if (typeof updaterOrValue === "function") {
      sorting.value = updaterOrValue(sorting.value);
    } else {
      sorting.value = updaterOrValue;
    }
  },
  onColumnFiltersChange: (updaterOrValue) => {
    if (typeof updaterOrValue === "function") {
      columnFilters.value = updaterOrValue(columnFilters.value);
    } else {
      columnFilters.value = updaterOrValue;
    }
  },
  onColumnVisibilityChange: (updaterOrValue) => {
    if (typeof updaterOrValue === "function") {
      columnVisibility.value = updaterOrValue(columnVisibility.value);
    } else {
      columnVisibility.value = updaterOrValue;
    }
  },
  onRowSelectionChange: (updaterOrValue) => {
    if (typeof updaterOrValue === "function") {
      rowSelection.value = updaterOrValue(rowSelection.value);
    } else {
      rowSelection.value = updaterOrValue;
    }
  },
  state: {
    get sorting() {
      return sorting.value;
    },
    get columnFilters() {
      return columnFilters.value;
    },
    get columnVisibility() {
      return columnVisibility.value;
    },
    get rowSelection() {
      return rowSelection.value;
    },
  },
});
</script>
