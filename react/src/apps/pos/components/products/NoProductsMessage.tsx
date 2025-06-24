import React from 'react';
import { Package } from 'lucide-react';

interface NoProductsMessageProps {
    searchQuery: string;
}

/**
 * NoProductsMessage Component
 * 
 * Displays a message when no products are found.
 * Shows different messages based on whether a search query is active.
 */
export function NoProductsMessage({ searchQuery }: NoProductsMessageProps) {
    return (
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
    );
}