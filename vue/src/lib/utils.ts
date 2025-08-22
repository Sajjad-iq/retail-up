import type { ClassValue } from "clsx"
import { clsx } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

/**
 * Formats date strings for display
 * @param dateString - ISO date string to format
 * @returns Formatted date string or 'N/A' if invalid
 */
export function formatDate(dateString: string): string {
  if (!dateString) return 'N/A'
  try {
    return new Date(dateString).toLocaleDateString()
  } catch {
    return 'Invalid Date'
  }
}


export function debounce(func: Function, wait: number) {
  let timeout: ReturnType<typeof setTimeout>;
  return (...args: any[]) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => func( ...args), wait);
  };
}   

export function valueUpdater<T>(
  updaterOrValue: T | ((old: T) => T),
  fn: (value: T) => void
) {
  if (typeof updaterOrValue === "function") {
    fn((updaterOrValue as (old: T) => T)(fn as any))
  } else {
    fn(updaterOrValue)
  }
}   