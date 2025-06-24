import React from 'react';
import { Tabs, TabsList, TabsTrigger } from '@/components/ui/tabs';

interface CategoryTabsProps {
    categories: string[];
    selectedCategory: string;
    onCategoryChange: (category: string) => void;
}

/**
 * CategoryTabs Component
 * 
 * Displays category tabs for filtering products by category.
 * Uses shadcn/ui Tabs component with a grid layout.
 */
export function CategoryTabs({
    categories,
    selectedCategory,
    onCategoryChange
}: CategoryTabsProps) {
    return (
        <div className="mb-4">
            <Tabs value={selectedCategory} onValueChange={onCategoryChange} className="w-full">
                <TabsList className="grid w-full grid-cols-4">
                    {categories.map((category) => (
                        <TabsTrigger key={category} value={category}>
                            {category}
                        </TabsTrigger>
                    ))}
                </TabsList>
            </Tabs>
        </div>
    );
} 