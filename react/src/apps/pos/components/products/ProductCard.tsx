import React from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { ShoppingCart, Package } from 'lucide-react';
import { formatPrice, isLowStock, isOutOfStock } from '../../lib/utils/pos-utils';
import type { Product } from '../../types/pos';

interface ProductCardProps {
    product: Product;
    onAddToCart: (product: Product) => void;
}

/**
 * ProductCard Component
 * 
 * Displays a single product in a card format with price, stock status, and add to cart functionality.
 * Shows product information including name, category, price, and stock level with appropriate badges.
 */
export function ProductCard({ product, onAddToCart }: ProductCardProps) {
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
        <Card className="h-full hover:shadow-md transition-shadow cursor-pointer">
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
                        onClick={() => onAddToCart(product)}
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
    );
} 