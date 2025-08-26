    import type { ColumnDef } from "@tanstack/vue-table";
import { h } from "vue";

import { ArrowUpDown } from "lucide-vue-next";
import { Badge } from "@/components/ui/badge";

import { Checkbox } from "@/components/ui/checkbox";
import { Button } from "@/components/ui/button";
import { CubeIcon } from "@heroicons/vue/24/outline";
import { EyeIcon, PencilIcon, TrashIcon } from "@heroicons/vue/24/outline";

import type { InventoryItem } from "@/types/global";

// Define the emit function type for actions
export type EmitFunctions = {
  view: (item: InventoryItem) => void;
  edit: (item: InventoryItem) => void;
  delete: (item: InventoryItem) => void;
};

// Function to create columns with emit functions
export const createColumns = (emit: EmitFunctions): ColumnDef<InventoryItem>[] => [
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
            onClick: () => emit.view(item),
            variant: "ghost",
            size: "sm",
            class: "h-8 w-8 p-0",
          },
          () => [h(EyeIcon, { class: "h-4 w-4" })]
        ),
        h(
          Button,
          {
            onClick: () => emit.edit(item),
            variant: "ghost",
            size: "sm",
            class: "h-8 w-8 p-0",
          },
          () => [h(PencilIcon, { class: "h-4 w-4" })]
        ),
        h(
          Button,
          {
            onClick: () => emit.delete(item),
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
