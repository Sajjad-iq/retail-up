import React, { useState, useMemo } from 'react';
import { Input } from '@/components/ui/input';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Search } from 'lucide-react';
import { useProducts, useCart } from '../../hooks/use-pos';
import { ProductCard, CategoryTabs, NoProductsMessage } from './';
import type { Product } from '../../types/pos';

interface ProductGridProps {
    /** CSS class name for styling */
    className?: string;
}

/**
 * ProductGrid Component
 * 
 * Displays a grid of products with search and category filtering functionality.
 * Uses category tabs for navigation and a responsive grid layout for product display.
 * 
 * @param props - Component props
 * @returns ProductGrid component
 */
export function ProductGrid({ className }: ProductGridProps) {
    const { products, categories } = useProducts();
    const { addToCart } = useCart();
    const [selectedCategory, setSelectedCategory] = useState<string>('All');
    const [searchQuery, setSearchQuery] = useState<string>('');

    /**
     * Filter products based on category and search query
     */
    const filteredProducts = useMemo(() => {
        let filtered = products;

        // Filter by category
        if (selectedCategory !== 'All') {
            filtered = filtered.filter(product => product.category === selectedCategory);
        }

        // Filter by search query
        if (searchQuery.trim()) {
            const query = searchQuery.toLowerCase();
            filtered = filtered.filter(product =>
                product.name.toLowerCase().includes(query) ||
                product.category.toLowerCase().includes(query) ||
                (product.barcode && product.barcode.toLowerCase().includes(query))
            );
        }

        return filtered;
    }, [products, selectedCategory, searchQuery]);

    /**
     * Handle adding product to cart
     */
    const handleAddToCart = (product: Product) => {
        addToCart(product);
    };

    /**
     * Handle category change
     */
    const handleCategoryChange = (category: string) => {
        setSelectedCategory(category);
    };

    /**
     * Handle search query change
     */
    const handleSearchChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setSearchQuery(event.target.value);
    };

    return (
        <div className={`h-full flex flex-col ${className}`}>
            {/* Search Bar */}
            <div className="mb-4">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-muted-foreground h-4 w-4" />
                    <Input
                        type="text"
                        placeholder="Search products..."
                        value={searchQuery}
                        onChange={handleSearchChange}
                        className="pl-10"
                    />
                </div>
            </div>

            {/* Category Tabs */}
            <CategoryTabs
                categories={categories}
                selectedCategory={selectedCategory}
                onCategoryChange={handleCategoryChange}
            />

            {/* Products Grid */}
            <ScrollArea className="flex-1">
                {filteredProducts.length === 0 ? (
                    <NoProductsMessage searchQuery={searchQuery} />
                ) : (
                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4 p-4">
                        {filteredProducts.map((product) => (
                            <ProductCard
                                key={product.id}
                                product={product}
                                onAddToCart={handleAddToCart}
                            />
                        ))}
                    </div>
                )}
            </ScrollArea>
        </div>
    );
}

export default ProductGrid;