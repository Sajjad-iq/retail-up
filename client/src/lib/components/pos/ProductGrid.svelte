<script lang="ts">
    import { products, cartOperations } from "$lib/stores/pos.js";
    import { Button } from "$lib/components/ui/button/index.js";
    import {
        Card,
        CardContent,
        CardDescription,
        CardHeader,
        CardTitle,
    } from "$lib/components/ui/card/index.js";
    import { Badge } from "$lib/components/ui/badge/index.js";
    import { ScrollArea } from "$lib/components/ui/scroll-area/index.js";
    import {
        Tabs,
        TabsContent,
        TabsList,
        TabsTrigger,
    } from "$lib/components/ui/tabs/index.js";
    import { ShoppingCart, Package } from "lucide-svelte";
    import type { Product } from "$lib/types.js";

    const categories = ["All", "Beverages", "Food", "Bakery"];
    let selectedCategory = "All";

    $: filteredProducts =
        selectedCategory === "All"
            ? $products
            : $products.filter((p) => p.category === selectedCategory);

    function addToCart(product: Product) {
        cartOperations.addItem(product);
    }

    function formatPrice(price: number): string {
        return `$${price.toFixed(2)}`;
    }
</script>

<div class="h-full flex flex-col">
    <div class="mb-4">
        <Tabs bind:value={selectedCategory} class="w-full">
            <TabsList class="grid w-full grid-cols-4">
                {#each categories as category}
                    <TabsTrigger value={category}>{category}</TabsTrigger>
                {/each}
            </TabsList>
        </Tabs>
    </div>

    <ScrollArea class="flex-1">
        <div class="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
            {#each filteredProducts as product (product.id)}
                <Card
                    class="h-full hover:shadow-md transition-shadow cursor-pointer"
                >
                    <CardHeader class="pb-2">
                        <div class="flex items-center justify-between">
                            <Badge variant="secondary" class="text-xs">
                                {product.category}
                            </Badge>
                            <Package class="h-4 w-4 text-muted-foreground" />
                        </div>
                        <CardTitle class="text-sm line-clamp-2"
                            >{product.name}</CardTitle
                        >
                        <CardDescription class="text-xs">
                            Stock: {product.stock}
                        </CardDescription>
                    </CardHeader>

                    <CardContent class="pt-0">
                        <div class="flex items-center justify-between">
                            <span class="text-lg font-bold text-primary">
                                {formatPrice(product.price)}
                            </span>
                            <Button
                                size="sm"
                                onclick={() => addToCart(product)}
                                disabled={product.stock === 0}
                                class="h-8 w-8 p-0"
                            >
                                <ShoppingCart class="h-4 w-4" />
                            </Button>
                        </div>

                        {#if product.stock <= 5 && product.stock > 0}
                            <Badge variant="destructive" class="mt-2 text-xs">
                                Low Stock
                            </Badge>
                        {:else if product.stock === 0}
                            <Badge variant="outline" class="mt-2 text-xs">
                                Out of Stock
                            </Badge>
                        {/if}
                    </CardContent>
                </Card>
            {/each}
        </div>
    </ScrollArea>
</div>

<style>
    .line-clamp-2 {
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
        overflow: hidden;
    }
</style>
