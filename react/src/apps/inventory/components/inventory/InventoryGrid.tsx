import React, { useState, useMemo } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
    Filter,
    SortAsc,
    SortDesc,
    Grid3X3,
    List,
    Search,
    Package2
} from 'lucide-react';

import { InventoryItemCard } from './InventoryItemCard';
import { InventoryItemTable } from './InventoryItemTable';
import { useInventoryItems, useInventoryCategories } from '../../hooks/use-inventory';
import type { InventoryFilters } from '../../types/inventory';

interface InventoryGridProps {
    searchQuery?: string;
}

/**
 * InventoryGrid Component
 * 
 * Displays inventory items in either grid or table view with filtering and sorting options.
 * Provides search, category filtering, status filtering, and sorting functionality.
 */
export function InventoryGrid({ searchQuery = '' }: InventoryGridProps) {
    const [viewMode, setViewMode] = useState<'grid' | 'table'>('grid');
    const [filters, setFilters] = useState<InventoryFilters>({
        query: searchQuery,
        category: 'all',
        status: undefined,
        stockLevel: 'all',
        sortBy: 'name',
        sortOrder: 'asc',
    });

    const { items, filterItems, loading } = useInventoryItems();
    const { categories } = useInventoryCategories();

    // Update filters when search query changes
    React.useEffect(() => {
        setFilters(prev => ({ ...prev, query: searchQuery }));
    }, [searchQuery]);

    // Filter and sort items
    const filteredItems = useMemo(() => {
        return filterItems(filters);
    }, [items, filters, filterItems]);

    const handleFilterChange = (key: keyof InventoryFilters, value: any) => {
        setFilters(prev => ({
            ...prev,
            [key]: value,
        }));
    };

    const toggleSortOrder = () => {
        setFilters(prev => ({
            ...prev,
            sortOrder: prev.sortOrder === 'asc' ? 'desc' : 'asc',
        }));
    };

    const clearFilters = () => {
        setFilters({
            query: searchQuery,
            category: 'all',
            status: undefined,
            stockLevel: 'all',
            sortBy: 'name',
            sortOrder: 'asc',
        });
    };

    if (loading) {
        return (
            <div className="flex items-center justify-center h-full">
                <div className="text-center">
                    <Package2 className="h-8 w-8 mx-auto mb-4 text-muted-foreground animate-pulse" />
                    <p className="text-muted-foreground">Loading inventory...</p>
                </div>
            </div>
        );
    }

    return (
        <div className="h-full flex flex-col">
            {/* Filters and Controls */}
            <div className="border-b bg-background p-4 space-y-4">
                {/* Search (mobile only) */}
                <div className="md:hidden">
                    <div className="relative">
                        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                        <Input
                            placeholder="Search inventory..."
                            value={filters.query || ''}
                            onChange={(e) => handleFilterChange('query', e.target.value)}
                            className="pl-9"
                        />
                    </div>
                </div>

                {/* Filters Row */}
                <div className="flex items-center gap-4 flex-wrap">
                    {/* Category Filter */}
                    <Select
                        value={filters.category || 'all'}
                        onValueChange={(value) => handleFilterChange('category', value)}
                    >
                        <SelectTrigger className="w-[150px]">
                            <SelectValue placeholder="Category" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Categories</SelectItem>
                            {categories.map((category) => (
                                <SelectItem key={category.id} value={category.name}>
                                    {category.name}
                                </SelectItem>
                            ))}
                        </SelectContent>
                    </Select>

                    {/* Status Filter */}
                    <Select
                        value={filters.status || 'all'}
                        onValueChange={(value) => handleFilterChange('status', value === 'all' ? undefined : value)}
                    >
                        <SelectTrigger className="w-[120px]">
                            <SelectValue placeholder="Status" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Status</SelectItem>
                            <SelectItem value="active">Active</SelectItem>
                            <SelectItem value="inactive">Inactive</SelectItem>
                            <SelectItem value="discontinued">Discontinued</SelectItem>
                        </SelectContent>
                    </Select>

                    {/* Stock Level Filter */}
                    <Select
                        value={filters.stockLevel || 'all'}
                        onValueChange={(value) => handleFilterChange('stockLevel', value)}
                    >
                        <SelectTrigger className="w-[130px]">
                            <SelectValue placeholder="Stock Level" />
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="all">All Levels</SelectItem>
                            <SelectItem value="normal">Normal</SelectItem>
                            <SelectItem value="low">Low Stock</SelectItem>
                            <SelectItem value="out">Out of Stock</SelectItem>
                        </SelectContent>
                    </Select>

                    {/* Sort Controls */}
                    <div className="flex items-center gap-2">
                        <Select
                            value={filters.sortBy || 'name'}
                            onValueChange={(value) => handleFilterChange('sortBy', value)}
                        >
                            <SelectTrigger className="w-[120px]">
                                <SelectValue placeholder="Sort by" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="name">Name</SelectItem>
                                <SelectItem value="stock">Stock</SelectItem>
                                <SelectItem value="price">Price</SelectItem>
                                <SelectItem value="updatedAt">Updated</SelectItem>
                            </SelectContent>
                        </Select>

                        <Button
                            variant="outline"
                            size="sm"
                            onClick={toggleSortOrder}
                            aria-label={`Sort ${filters.sortOrder === 'asc' ? 'descending' : 'ascending'}`}
                        >
                            {filters.sortOrder === 'asc' ? (
                                <SortAsc className="h-4 w-4" />
                            ) : (
                                <SortDesc className="h-4 w-4" />
                            )}
                        </Button>
                    </div>

                    {/* View Mode Toggle */}
                    <div className="flex items-center gap-1 ml-auto">
                        <Button
                            variant={viewMode === 'grid' ? 'default' : 'outline'}
                            size="sm"
                            onClick={() => setViewMode('grid')}
                            aria-label="Grid view"
                        >
                            <Grid3X3 className="h-4 w-4" />
                        </Button>
                        <Button
                            variant={viewMode === 'table' ? 'default' : 'outline'}
                            size="sm"
                            onClick={() => setViewMode('table')}
                            aria-label="Table view"
                        >
                            <List className="h-4 w-4" />
                        </Button>
                    </div>

                    {/* Clear Filters */}
                    <Button
                        variant="ghost"
                        size="sm"
                        onClick={clearFilters}
                        className="text-muted-foreground"
                    >
                        <Filter className="h-4 w-4 mr-1" />
                        Clear
                    </Button>
                </div>

                {/* Active Filters Display */}
                <div className="flex items-center gap-2 flex-wrap">
                    {filters.category && filters.category !== 'all' && (
                        <Badge variant="secondary" className="text-xs">
                            Category: {filters.category}
                        </Badge>
                    )}
                    {filters.status && (
                        <Badge variant="secondary" className="text-xs">
                            Status: {filters.status}
                        </Badge>
                    )}
                    {filters.stockLevel && filters.stockLevel !== 'all' && (
                        <Badge variant="secondary" className="text-xs">
                            Stock: {filters.stockLevel}
                        </Badge>
                    )}
                    {filters.query && (
                        <Badge variant="secondary" className="text-xs">
                            Search: "{filters.query}"
                        </Badge>
                    )}
                </div>
            </div>

            {/* Items Display */}
            <div className="flex-1 overflow-auto p-4">
                {filteredItems.length === 0 ? (
                    <div className="flex items-center justify-center h-full">
                        <div className="text-center">
                            <Package2 className="h-12 w-12 mx-auto mb-4 text-muted-foreground" />
                            <h3 className="text-lg font-semibold mb-2">No items found</h3>
                            <p className="text-muted-foreground mb-4">
                                Try adjusting your filters or search terms.
                            </p>
                            <Button onClick={clearFilters}>
                                Clear Filters
                            </Button>
                        </div>
                    </div>
                ) : (
                    <>
                        {/* Results Count */}
                        <div className="mb-4">
                            <p className="text-sm text-muted-foreground">
                                Showing {filteredItems.length} of {items.length} items
                            </p>
                        </div>

                        {/* Items Grid/Table */}
                        {viewMode === 'grid' ? (
                            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
                                {filteredItems.map((item) => (
                                    <InventoryItemCard
                                        key={item.id}
                                        item={item}
                                    />
                                ))}
                            </div>
                        ) : (
                            <InventoryItemTable items={filteredItems} />
                        )}
                    </>
                )}
            </div>
        </div>
    );
} 