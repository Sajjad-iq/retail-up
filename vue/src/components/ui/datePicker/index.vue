<script setup lang="ts">
import type { DateValue } from "@internationalized/date";
import { DateFormatter, getLocalTimeZone, parseDate } from "@internationalized/date";
import { Calendar as CalendarIcon } from "lucide-vue-next";

import { computed } from "vue";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { Calendar } from "@/components/ui/calendar";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";

interface Props {
  modelValue?: DateValue | string | null;
  placeholder?: string;
}

interface Emits {
  (e: "update:modelValue", value: string | null): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const df = new DateFormatter("en-US", {
  dateStyle: "long",
});

// Convert string dates to DateValue for display
const displayValue = computed((): DateValue | undefined => {
  if (!props.modelValue) return undefined;
  if (typeof props.modelValue === "string") {
    try {
      // Convert ISO string to DateValue
      return parseDate(props.modelValue);
    } catch {
      return undefined;
    }
  }
  return props.modelValue;
});

const handleDateChange = (value: DateValue | undefined) => {
  if (value) {
    // Convert DateValue to ISO string for the form
    const date = value.toDate(getLocalTimeZone());
    emit("update:modelValue", date.toISOString());
  } else {
    emit("update:modelValue", null);
  }
};
</script>

<template>
  <Popover>
    <PopoverTrigger as-child>
      <Button
        variant="outline"
        :class="
          cn(
            'w-[280px] justify-start text-left font-normal',
            !displayValue && 'text-muted-foreground'
          )
        "
      >
        <CalendarIcon class="mr-2 h-4 w-4" />
        {{
          displayValue
            ? df.format(displayValue.toDate(getLocalTimeZone()))
            : placeholder || "Pick a date"
        }}
      </Button>
    </PopoverTrigger>
    <PopoverContent class="w-auto p-0">
      <Calendar :model-value="displayValue" @update:model-value="handleDateChange" initial-focus />
    </PopoverContent>
  </Popover>
</template>
