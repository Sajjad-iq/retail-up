<script lang="ts">
    import { cart, transactions } from "$lib/stores/pos.js";
    import ProductGrid from "./ProductGrid.svelte";
    import Cart from "./Cart.svelte";
    import PaymentDialog from "./PaymentDialog.svelte";
    import { Button } from "$lib/components/ui/button/index.js";
    import {
        Card,
        CardContent,
        CardHeader,
        CardTitle,
    } from "$lib/components/ui/card/index.js";
    import { Input } from "$lib/components/ui/input/index.js";
    import { Avatar, AvatarFallback } from "$lib/components/ui/avatar/index.js";
    import { Badge } from "$lib/components/ui/badge/index.js";
    import {
        Store,
        Search,
        CreditCard,
        Receipt,
        User,
        Clock,
        DollarSign,
    } from "lucide-svelte";
    import type { Transaction } from "$lib/types.js";

    let searchQuery = "";
    let showPaymentDialog = false;

    $: cartTotal = $cart.reduce(
        (sum, item) => sum + item.product.price * item.quantity,
        0,
    );
    $: todaysTransactions = $transactions.filter(
        (t) =>
            new Date(t.timestamp).toDateString() === new Date().toDateString(),
    );
    $: todaysRevenue = todaysTransactions.reduce((sum, t) => sum + t.total, 0);

    function formatPrice(price: number): string {
        return `$${price.toFixed(2)}`;
    }

    function formatTime(date: Date): string {
        return date.toLocaleTimeString([], {
            hour: "2-digit",
            minute: "2-digit",
        });
    }

    function openPayment() {
        if ($cart.length > 0) {
            showPaymentDialog = true;
        }
    }

    function onPaymentComplete(event: CustomEvent<Transaction>) {
        console.log("Payment completed:", event.detail);
        showPaymentDialog = false;
    }
</script>

<div class="h-screen bg-background">
    <!-- Header -->
    <header class="border-b bg-card">
        <div class="flex items-center justify-between p-4">
            <div class="flex items-center gap-3">
                <div class="flex items-center gap-2">
                    <Store class="h-6 w-6 text-primary" />
                    <h1 class="text-xl font-bold">RetailUp POS</h1>
                </div>
                <Badge variant="secondary" class="text-xs">v1.0.0</Badge>
            </div>

            <div class="flex items-center gap-4">
                <!-- Today's Stats -->
                <div class="hidden md:flex items-center gap-6">
                    <div class="text-center">
                        <p class="text-xs text-muted-foreground">
                            Today's Sales
                        </p>
                        <p class="text-sm font-semibold">
                            {todaysTransactions.length}
                        </p>
                    </div>
                    <div class="text-center">
                        <p class="text-xs text-muted-foreground">Revenue</p>
                        <p class="text-sm font-semibold">
                            {formatPrice(todaysRevenue)}
                        </p>
                    </div>
                </div>

                <!-- User Avatar -->
                <Avatar class="h-8 w-8">
                    <AvatarFallback>
                        <User class="h-4 w-4" />
                    </AvatarFallback>
                </Avatar>
            </div>
        </div>
    </header>

    <!-- Main Content -->
    <div class="flex h-[calc(100vh-73px)]">
        <!-- Left Side - Products -->
        <div class="flex-1 flex flex-col">
            <!-- Search Bar -->
            <div class="p-4 border-b">
                <div class="relative">
                    <Search
                        class="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground"
                    />
                    <Input
                        placeholder="Search products..."
                        bind:value={searchQuery}
                        class="pl-10"
                    />
                </div>
            </div>

            <!-- Product Grid -->
            <div class="flex-1 p-4">
                <ProductGrid />
            </div>
        </div>

        <!-- Right Side - Cart & Actions -->
        <div class="w-96 border-l bg-muted/20 flex flex-col">
            <!-- Cart -->
            <div class="flex-1 p-4">
                <Cart />
            </div>

            <!-- Checkout Actions -->
            <div class="border-t p-4 space-y-3">
                <Button
                    class="w-full"
                    size="lg"
                    onclick={openPayment}
                    disabled={$cart.length === 0}
                >
                    <CreditCard class="mr-2 h-4 w-4" />
                    Checkout {$cart.length > 0
                        ? formatPrice(cartTotal * 1.08)
                        : ""}
                </Button>

                <!-- Quick Actions -->
                <div class="grid grid-cols-2 gap-2">
                    <Button variant="outline" size="sm" disabled>
                        <Receipt class="mr-2 h-4 w-4" />
                        Hold
                    </Button>
                    <Button variant="outline" size="sm" disabled>
                        <User class="mr-2 h-4 w-4" />
                        Customer
                    </Button>
                </div>
            </div>

            <!-- Recent Transactions -->
            {#if todaysTransactions.length > 0}
                <div class="border-t p-4">
                    <h3
                        class="text-sm font-semibold mb-3 flex items-center gap-2"
                    >
                        <Clock class="h-4 w-4" />
                        Recent Transactions
                    </h3>
                    <div class="space-y-2 max-h-32 overflow-y-auto">
                        {#each todaysTransactions.slice(0, 3) as transaction}
                            <div
                                class="flex items-center justify-between text-xs p-2 bg-background rounded border"
                            >
                                <div>
                                    <p class="font-medium">{transaction.id}</p>
                                    <p class="text-muted-foreground">
                                        {formatTime(transaction.timestamp)}
                                    </p>
                                </div>
                                <div class="text-right">
                                    <p class="font-semibold">
                                        {formatPrice(transaction.total)}
                                    </p>
                                    <p class="text-muted-foreground capitalize">
                                        {transaction.paymentMethod}
                                    </p>
                                </div>
                            </div>
                        {/each}
                    </div>
                </div>
            {/if}
        </div>
    </div>

    <!-- Payment Dialog -->
    <PaymentDialog
        bind:open={showPaymentDialog}
        on:paymentComplete={onPaymentComplete}
    />
</div>
