<script lang="ts">
    import { cart, cartOperations } from "$lib/stores/pos.js";
    import { Button } from "$lib/components/ui/button/index.js";
    import {
        Card,
        CardContent,
        CardHeader,
        CardTitle,
    } from "$lib/components/ui/card/index.js";
    import { Input } from "$lib/components/ui/input/index.js";
    import { ScrollArea } from "$lib/components/ui/scroll-area/index.js";
    import { Separator } from "$lib/components/ui/separator/index.js";
    import {
        Table,
        TableBody,
        TableCell,
        TableHead,
        TableHeader,
        TableRow,
    } from "$lib/components/ui/table/index.js";
    import { Trash2, Plus, Minus } from "lucide-svelte";

    $: subtotal = $cart.reduce(
        (sum, item) => sum + item.product.price * item.quantity,
        0,
    );
    $: tax = subtotal * 0.08; // 8% tax
    $: total = subtotal + tax;

    function updateQuantity(productId: string, quantity: number) {
        cartOperations.updateQuantity(productId, quantity);
    }

    function removeItem(productId: string) {
        cartOperations.removeItem(productId);
    }

    function clearCart() {
        cartOperations.clear();
    }

    function formatPrice(price: number): string {
        return `$${price.toFixed(2)}`;
    }
</script>

<Card class="h-full flex flex-col">
    <CardHeader class="pb-4">
        <div class="flex items-center justify-between">
            <CardTitle class="text-lg">Current Order</CardTitle>
            {#if $cart.length > 0}
                <Button variant="outline" size="sm" onclick={clearCart}>
                    Clear All
                </Button>
            {/if}
        </div>
    </CardHeader>

    <CardContent class="flex-1 flex flex-col p-0">
        {#if $cart.length === 0}
            <div
                class="flex-1 flex items-center justify-center text-center p-8"
            >
                <div class="text-muted-foreground">
                    <p class="text-lg mb-2">Cart is empty</p>
                    <p class="text-sm">Add items from the product grid</p>
                </div>
            </div>
        {:else}
            <ScrollArea class="flex-1 px-6">
                <Table>
                    <TableHeader>
                        <TableRow>
                            <TableHead>Item</TableHead>
                            <TableHead class="text-center">Qty</TableHead>
                            <TableHead class="text-right">Price</TableHead>
                            <TableHead class="w-12"></TableHead>
                        </TableRow>
                    </TableHeader>
                    <TableBody>
                        {#each $cart as item (item.product.id)}
                            <TableRow>
                                <TableCell class="font-medium">
                                    <div>
                                        <p class="font-semibold">
                                            {item.product.name}
                                        </p>
                                        <p
                                            class="text-sm text-muted-foreground"
                                        >
                                            {formatPrice(item.product.price)} each
                                        </p>
                                    </div>
                                </TableCell>
                                <TableCell class="text-center">
                                    <div
                                        class="flex items-center justify-center gap-2"
                                    >
                                        <Button
                                            variant="outline"
                                            size="sm"
                                            class="h-6 w-6 p-0"
                                            onclick={() =>
                                                updateQuantity(
                                                    item.product.id,
                                                    item.quantity - 1,
                                                )}
                                        >
                                            <Minus class="h-3 w-3" />
                                        </Button>
                                        <Input
                                            type="number"
                                            min="1"
                                            value={item.quantity}
                                            oninput={(e) =>
                                                updateQuantity(
                                                    item.product.id,
                                                    parseInt(
                                                        e.currentTarget.value,
                                                    ) || 1,
                                                )}
                                            class="w-16 h-6 text-center text-xs"
                                        />
                                        <Button
                                            variant="outline"
                                            size="sm"
                                            class="h-6 w-6 p-0"
                                            onclick={() =>
                                                updateQuantity(
                                                    item.product.id,
                                                    item.quantity + 1,
                                                )}
                                        >
                                            <Plus class="h-3 w-3" />
                                        </Button>
                                    </div>
                                </TableCell>
                                <TableCell class="text-right font-semibold">
                                    {formatPrice(
                                        item.product.price * item.quantity,
                                    )}
                                </TableCell>
                                <TableCell>
                                    <Button
                                        variant="ghost"
                                        size="sm"
                                        class="h-6 w-6 p-0 text-destructive hover:text-destructive"
                                        onclick={() =>
                                            removeItem(item.product.id)}
                                    >
                                        <Trash2 class="h-3 w-3" />
                                    </Button>
                                </TableCell>
                            </TableRow>
                        {/each}
                    </TableBody>
                </Table>
            </ScrollArea>

            <div class="border-t p-6">
                <div class="space-y-2">
                    <div class="flex justify-between text-sm">
                        <span>Subtotal:</span>
                        <span>{formatPrice(subtotal)}</span>
                    </div>
                    <div class="flex justify-between text-sm">
                        <span>Tax (8%):</span>
                        <span>{formatPrice(tax)}</span>
                    </div>
                    <Separator />
                    <div class="flex justify-between text-lg font-bold">
                        <span>Total:</span>
                        <span>{formatPrice(total)}</span>
                    </div>
                </div>
            </div>
        {/if}
    </CardContent>
</Card>
