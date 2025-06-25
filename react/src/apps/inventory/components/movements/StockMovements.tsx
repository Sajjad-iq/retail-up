import React from 'react';
import { Package, TrendingUp } from 'lucide-react';

/**
 * StockMovements Component
 * 
 * Displays and manages stock movements and transactions.
 * Shows recent stock movements, allows creating new movements, and provides movement history.
 */
export function StockMovements() {
    return (
        <div className="h-full flex items-center justify-center">
            <div className="text-center">
                <TrendingUp className="h-12 w-12 mx-auto mb-4 text-muted-foreground" />
                <h3 className="text-lg font-semibold mb-2">Stock Movements</h3>
                <p className="text-muted-foreground">
                    Stock movement tracking coming soon...
                </p>
            </div>
        </div>
    );
} 