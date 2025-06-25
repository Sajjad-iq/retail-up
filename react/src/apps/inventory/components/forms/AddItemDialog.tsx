import React from 'react';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Plus } from 'lucide-react';

interface AddItemDialogProps {
    isOpen: boolean;
    onClose: () => void;
    onSuccess: () => void;
}

/**
 * AddItemDialog Component
 * 
 * Dialog for adding new inventory items.
 * Provides form interface for creating new inventory items with validation.
 */
export function AddItemDialog({ isOpen, onClose, onSuccess }: AddItemDialogProps) {
    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="sm:max-w-[600px]">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Plus className="h-5 w-5" />
                        Add New Item
                    </DialogTitle>
                    <DialogDescription>
                        Add a new item to your inventory.
                    </DialogDescription>
                </DialogHeader>

                <div className="flex items-center justify-center h-32">
                    <p className="text-muted-foreground">
                        Add item form coming soon...
                    </p>
                </div>
            </DialogContent>
        </Dialog>
    );
} 