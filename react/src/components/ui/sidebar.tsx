import * as React from "react"
import { Slot } from "@radix-ui/react-slot"
import { cn } from "@/lib/utils"
import { cva, type VariantProps } from "class-variance-authority"

const sidebarVariants = cva(
    "flex h-screen w-64 flex-col border-r bg-background",
    {
        variants: {
            variant: {
                default: "border-border",
                ghost: "border-transparent",
            },
            size: {
                default: "w-64",
                sm: "w-48",
                lg: "w-80",
            },
        },
        defaultVariants: {
            variant: "default",
            size: "default",
        },
    }
)

export interface SidebarProps
    extends React.HTMLAttributes<HTMLDivElement>,
    VariantProps<typeof sidebarVariants> {
    asChild?: boolean
}

const Sidebar = React.forwardRef<HTMLDivElement, SidebarProps>(
    ({ className, variant, size, asChild = false, ...props }, ref) => {
        const Comp = asChild ? Slot : "div"
        return (
            <Comp
                className={cn(sidebarVariants({ variant, size, className }))}
                ref={ref}
                {...props}
            />
        )
    }
)
Sidebar.displayName = "Sidebar"

const SidebarHeader = React.forwardRef<
    HTMLDivElement,
    React.HTMLAttributes<HTMLDivElement>
>(({ className, ...props }, ref) => (
    <div
        ref={ref}
        className={cn("flex h-16 items-center px-6 border-b border-border", className)}
        {...props}
    />
))
SidebarHeader.displayName = "SidebarHeader"

const SidebarContent = React.forwardRef<
    HTMLDivElement,
    React.HTMLAttributes<HTMLDivElement>
>(({ className, ...props }, ref) => (
    <div
        ref={ref}
        className={cn("flex-1 overflow-auto p-4", className)}
        {...props}
    />
))
SidebarContent.displayName = "SidebarContent"

const SidebarFooter = React.forwardRef<
    HTMLDivElement,
    React.HTMLAttributes<HTMLDivElement>
>(({ className, ...props }, ref) => (
    <div
        ref={ref}
        className={cn("p-4 border-t border-border", className)}
        {...props}
    />
))
SidebarFooter.displayName = "SidebarFooter"

const SidebarNav = React.forwardRef<
    HTMLElement,
    React.HTMLAttributes<HTMLElement>
>(({ className, ...props }, ref) => (
    <nav
        ref={ref}
        className={cn("flex flex-col space-y-1", className)}
        {...props}
    />
))
SidebarNav.displayName = "SidebarNav"

const SidebarNavItem = React.forwardRef<
    HTMLButtonElement,
    React.ButtonHTMLAttributes<HTMLButtonElement> & {
        asChild?: boolean
        active?: boolean
    }
>(({ className, asChild = false, active = false, ...props }, ref) => {
    const Comp = asChild ? Slot : "button"
    return (
        <Comp
            className={cn(
                "flex items-center rounded-lg px-3 py-2 text-sm font-medium transition-colors",
                "hover:bg-accent hover:text-accent-foreground",
                "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
                active
                    ? "bg-accent text-accent-foreground"
                    : "text-muted-foreground",
                className
            )}
            ref={ref}
            {...props}
        />
    )
})
SidebarNavItem.displayName = "SidebarNavItem"

export {
    Sidebar,
    SidebarHeader,
    SidebarContent,
    SidebarFooter,
    SidebarNav,
    SidebarNavItem,
    sidebarVariants,
} 