import React, { useState, useMemo } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ShoppingCart, Package } from 'lucide-react';
import { useProducts, useCart } from '../hooks/use-pos';
import { formatPrice, isLowStock, isOutOfStock } from '../lib/utils/pos-utils';
import type { Product } from '../types/pos';

interface ProductGridProps {
    /** Search query to filter products */
    searchQuery?: string;
    /** CSS class name for styling */
    className?: string;
}

/**
 * ProductGrid Component
 * 
 * Displays products in a grid layout with category filtering and search functionality.
 * Allows users to add products to cart and shows stock status.
 * 
 * @param props - Component props
 * @returns ProductGrid component
 */
export function ProductGrid({ searchQuery = '', className }: ProductGridProps) {
    const { products, categories, getProductsByCategory } = useProducts();
    const { addToCart } = useCart();
    const [selectedCategory, setSelectedCategory] = useState<string>('All');

    /**
     * Filter products based on category and search query
     */
    const filteredProducts = useMemo(() => {
        let filtered = getProductsByCategory(selectedCategory);

        if (searchQuery.trim()) {
            const searchTerm = searchQuery.toLowerCase();
            filtered = filtered.filter(product =>
                product.name.toLowerCase().includes(searchTerm) ||
                product.category.toLowerCase().includes(searchTerm) ||
                product.barcode?.includes(searchTerm)
            );
        }

        return filtered;
    }, [selectedCategory, searchQuery, getProductsByCategory]);

    /**
     * Handle adding product to cart
     */
    const handleAddToCart = (product: Product) => {
        if (product.stock > 0) {
            addToCart(product);
        }
    };

    /**
     * Get stock status badge for a product
     */
    const getStockBadge = (product: Product) => {
        if (isOutOfStock(product.stock)) {
            return (
                <Badge variant="outline" className="mt-2 text-xs">
                    Out of Stock
                </Badge>
            );
        }

        if (isLowStock(product.stock)) {
            return (
                <Badge variant="destructive" className="mt-2 text-xs">
                    Low Stock
                </Badge>
            );
        }

        return null;
    };

    return (
        <div className={`h-full flex flex-col ${className}`}>
            {/* Category Tabs */}
            <div className="mb-4">
                <Tabs value={selectedCategory} onValueChange={setSelectedCategory} className="w-full">
                    <TabsList className="grid w-full grid-cols-4">
                        {categories.map((category) => (
                            <TabsTrigger key={category} value={category}>
                                {category}
                            </TabsTrigger>
                        ))}
                    </TabsList>
                </Tabs>
            </div>

            {/* Products Grid */}
            <ScrollArea className="flex-1">
                <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
                    {filteredProducts.map((product) => (
                        <Card
                            key={product.id}
                            className="h-full hover:shadow-md transition-shadow cursor-pointer"
                        >
                            <CardHeader className="pb-2">
                                <div className="flex items-center justify-between">
                                    <Badge variant="secondary" className="text-xs">
                                        {product.category}
                                    </Badge>
                                    <Package className="h-4 w-4 text-muted-foreground" />
                                </div>
                                <CardTitle className="text-sm line-clamp-2">
                                    {product.name}
                                </CardTitle>
                                <CardDescription className="text-xs">
                                    Stock: {product.stock}
                                </CardDescription>
                            </CardHeader>

                            <CardContent className="pt-0">
                                <div className="flex items-center justify-between">
                                    <span className="text-lg font-bold text-primary">
                                        {formatPrice(product.price)}
                                    </span>
                                    <Button
                                        size="sm"
                                        onClick={() => handleAddToCart(product)}
                                        disabled={isOutOfStock(product.stock)}
                                        className="h-8 w-8 p-0"
                                        aria-label={`Add ${product.name} to cart`}
                                    >
                                        <ShoppingCart className="h-4 w-4" />
                                    </Button>
                                </div>

                                {getStockBadge(product)}
                            </CardContent>
                        </Card>
                    ))}
                </div>

                {/* No products found message */}
                {filteredProducts.length === 0 && (
                    <div className="flex items-center justify-center h-64">
                        <div className="text-center text-muted-foreground">
                            <Package className="h-12 w-12 mx-auto mb-4 opacity-50" />
                            <p className="text-lg mb-2">No products found</p>
                            <p className="text-sm">
                                {searchQuery.trim()
                                    ? 'Try adjusting your search terms'
                                    : 'No products available in this category'
                                }
                            </p>
                        </div>
                    </div>
                )}
            </ScrollArea>
        </div>
    );
}

export default ProductGrid; 