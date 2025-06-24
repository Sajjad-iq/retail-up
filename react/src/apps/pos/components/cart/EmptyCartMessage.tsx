import React from 'react';

/**
 * EmptyCartMessage Component
 * 
 * Displays a message when the cart is empty, encouraging users to add items.
 */
export function EmptyCartMessage() {
    return (
        <div className="flex-1 flex items-center justify-center text-center p-8">
            <div className="text-muted-foreground">
                <p className="text-lg mb-2">Cart is empty</p>
                <p className="text-sm">Add items from the product grid</p>
            </div>
        </div>
    );
} 