<script lang="ts">
    import { cart, transactionOperations } from "$lib/stores/pos.js";
    import { Button } from "$lib/components/ui/button/index.js";
    import {
        Dialog,
        DialogContent,
        DialogHeader,
        DialogTitle,
        DialogDescription,
    } from "$lib/components/ui/dialog/index.js";
    import * as Select from "$lib/components/ui/select/index.js";
    import { Separator } from "$lib/components/ui/separator/index.js";
    import {
        CreditCard,
        DollarSign,
        Smartphone,
        CheckCircle,
        Loader2,
    } from "lucide-svelte";
    import type { PaymentMethod, Transaction } from "$lib/types.js";
    import { createEventDispatcher } from "svelte";

    export let open = false;

    const dispatch = createEventDispatcher();

    let selectedPaymentMethod: PaymentMethod = "cash";
    let isProcessing = false;
    let paymentComplete = false;
    let completedTransaction: Transaction | null = null;

    $: subtotal = $cart.reduce(
        (sum, item) => sum + item.product.price * item.quantity,
        0,
    );
    $: tax = subtotal * 0.08;
    $: total = subtotal + tax;

    function formatPrice(price: number): string {
        return `$${price.toFixed(2)}`;
    }

    async function processPayment() {
        if ($cart.length === 0) return;

        isProcessing = true;
        try {
            // Simulate payment processing delay
            await new Promise((resolve) => setTimeout(resolve, 2000));

            const transaction = await transactionOperations.processPayment(
                selectedPaymentMethod,
            );
            completedTransaction = transaction;
            paymentComplete = true;

            dispatch("paymentComplete", transaction);
        } catch (error) {
            console.error("Payment failed:", error);
        } finally {
            isProcessing = false;
        }
    }

    function closeDialog() {
        open = false;
        paymentComplete = false;
        completedTransaction = null;
        selectedPaymentMethod = "cash";
    }

    function printReceipt() {
        // In a real app, this would interface with a receipt printer
        alert("Receipt sent to printer!");
    }

    const paymentMethods = [
        { value: "cash", label: "Cash", icon: DollarSign },
        { value: "card", label: "Credit/Debit Card", icon: CreditCard },
        { value: "mobile", label: "Mobile Payment", icon: Smartphone },
    ];
</script>

<Dialog bind:open>
    <DialogContent class="sm:max-w-md">
        {#if !paymentComplete}
            <DialogHeader>
                <DialogTitle>Process Payment</DialogTitle>
                <DialogDescription>
                    Complete the transaction for {$cart.length} item{$cart.length !==
                    1
                        ? "s"
                        : ""}
                </DialogDescription>
            </DialogHeader>

            <div class="space-y-6">
                <!-- Order Summary -->
                <div class="space-y-2">
                    <h4 class="font-semibold">Order Summary</h4>
                    <div class="space-y-1 text-sm">
                        <div class="flex justify-between">
                            <span>Subtotal:</span>
                            <span>{formatPrice(subtotal)}</span>
                        </div>
                        <div class="flex justify-between">
                            <span>Tax (8%):</span>
                            <span>{formatPrice(tax)}</span>
                        </div>
                        <Separator />
                        <div class="flex justify-between font-bold text-lg">
                            <span>Total:</span>
                            <span>{formatPrice(total)}</span>
                        </div>
                    </div>
                </div>

                <!-- Payment Method Selection -->
                <div class="space-y-3">
                    <h4 class="font-semibold">Payment Method</h4>
                    <Select.Root
                        type="single"
                        bind:value={selectedPaymentMethod}
                    >
                        <Select.Trigger>
                            {#if selectedPaymentMethod}
                                {@const method = paymentMethods.find(
                                    (m) => m.value === selectedPaymentMethod,
                                )}
                                {#if method}
                                    <div class="flex items-center gap-2">
                                        <svelte:component
                                            this={method.icon}
                                            class="h-4 w-4"
                                        />
                                        {method.label}
                                    </div>
                                {/if}
                            {:else}
                                Select payment method
                            {/if}
                        </Select.Trigger>
                        <Select.Content>
                            {#each paymentMethods as method}
                                <Select.Item value={method.value}>
                                    <div class="flex items-center gap-2">
                                        <svelte:component
                                            this={method.icon}
                                            class="h-4 w-4"
                                        />
                                        {method.label}
                                    </div>
                                </Select.Item>
                            {/each}
                        </Select.Content>
                    </Select.Root>
                </div>

                <!-- Action Buttons -->
                <div class="flex gap-3">
                    <Button
                        variant="outline"
                        class="flex-1"
                        onclick={closeDialog}
                        disabled={isProcessing}
                    >
                        Cancel
                    </Button>
                    <Button
                        class="flex-1"
                        onclick={processPayment}
                        disabled={!selectedPaymentMethod ||
                            $cart.length === 0 ||
                            isProcessing}
                    >
                        {#if isProcessing}
                            <Loader2 class="mr-2 h-4 w-4 animate-spin" />
                            Processing...
                        {:else}
                            Pay {formatPrice(total)}
                        {/if}
                    </Button>
                </div>
            </div>
        {:else}
            <!-- Payment Success -->
            <DialogHeader>
                <DialogTitle class="flex items-center gap-2 text-green-600">
                    <CheckCircle class="h-5 w-5" />
                    Payment Successful
                </DialogTitle>
                <DialogDescription>
                    Transaction completed successfully
                </DialogDescription>
            </DialogHeader>

            {#if completedTransaction}
                <div class="space-y-4">
                    <div
                        class="text-center p-6 bg-green-50 rounded-lg border border-green-200"
                    >
                        <h3 class="text-lg font-bold text-green-800 mb-2">
                            {formatPrice(completedTransaction.total)}
                        </h3>
                        <p class="text-sm text-green-600">
                            Transaction ID: {completedTransaction.id}
                        </p>
                        <p class="text-xs text-green-500 mt-1">
                            {completedTransaction.timestamp.toLocaleString()}
                        </p>
                    </div>

                    <div class="space-y-2 text-sm">
                        <div class="flex justify-between">
                            <span>Payment Method:</span>
                            <span class="capitalize"
                                >{completedTransaction.paymentMethod}</span
                            >
                        </div>
                        <div class="flex justify-between">
                            <span>Items:</span>
                            <span>{completedTransaction.items.length}</span>
                        </div>
                    </div>

                    <div class="flex gap-3">
                        <Button
                            variant="outline"
                            class="flex-1"
                            onclick={printReceipt}
                        >
                            Print Receipt
                        </Button>
                        <Button class="flex-1" onclick={closeDialog}>
                            New Transaction
                        </Button>
                    </div>
                </div>
            {/if}
        {/if}
    </DialogContent>
</Dialog>
