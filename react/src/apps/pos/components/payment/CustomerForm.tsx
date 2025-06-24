import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { User, X } from 'lucide-react';
import type { Customer } from '../../types/pos';

interface CustomerFormProps {
    customer: Customer | null;
    onCustomerChange: (customer: Customer | null) => void;
}

/**
 * CustomerForm Component
 * 
 * Allows adding or editing customer information for the transaction.
 * Provides form fields for name, email, and phone number.
 */
export function CustomerForm({ customer, onCustomerChange }: CustomerFormProps) {
    const [isEditing, setIsEditing] = useState(false);
    const [formData, setFormData] = useState({
        name: customer?.name || '',
        email: customer?.email || '',
        phone: customer?.phone || '',
    });

    const handleSave = () => {
        if (formData.name.trim()) {
            onCustomerChange({
                id: customer?.id || `customer-${Date.now()}`,
                name: formData.name,
                email: formData.email || undefined,
                phone: formData.phone || undefined,
            });
        }
        setIsEditing(false);
    };

    const handleCancel = () => {
        setFormData({
            name: customer?.name || '',
            email: customer?.email || '',
            phone: customer?.phone || '',
        });
        setIsEditing(false);
    };

    const handleRemoveCustomer = () => {
        onCustomerChange(null);
        setFormData({ name: '', email: '', phone: '' });
    };

    if (!customer && !isEditing) {
        return (
            <div className="space-y-4">
                <h4 className="font-medium text-sm">Customer Information</h4>
                <Button
                    variant="outline"
                    onClick={() => setIsEditing(true)}
                    className="w-full"
                >
                    <User className="h-4 w-4 mr-2" />
                    Add Customer
                </Button>
            </div>
        );
    }

    if (isEditing) {
        return (
            <div className="space-y-4">
                <h4 className="font-medium text-sm">Customer Information</h4>
                <Card>
                    <CardHeader>
                        <CardTitle className="text-sm">Customer Details</CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-3">
                        <div>
                            <Label htmlFor="customer-name" className="text-xs">Name *</Label>
                            <Input
                                id="customer-name"
                                value={formData.name}
                                onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                placeholder="Customer name"
                                className="h-8"
                            />
                        </div>
                        <div>
                            <Label htmlFor="customer-email" className="text-xs">Email</Label>
                            <Input
                                id="customer-email"
                                type="email"
                                value={formData.email}
                                onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                                placeholder="customer@example.com"
                                className="h-8"
                            />
                        </div>
                        <div>
                            <Label htmlFor="customer-phone" className="text-xs">Phone</Label>
                            <Input
                                id="customer-phone"
                                value={formData.phone}
                                onChange={(e) => setFormData({ ...formData, phone: e.target.value })}
                                placeholder="Phone number"
                                className="h-8"
                            />
                        </div>
                        <div className="flex gap-2">
                            <Button size="sm" onClick={handleSave} disabled={!formData.name.trim()}>
                                Save
                            </Button>
                            <Button variant="outline" size="sm" onClick={handleCancel}>
                                Cancel
                            </Button>
                        </div>
                    </CardContent>
                </Card>
            </div>
        );
    }

    return (
        <div className="space-y-4">
            <h4 className="font-medium text-sm">Customer Information</h4>
            <Card>
                <CardContent className="p-3">
                    <div className="flex items-start justify-between">
                        <div className="flex-1">
                            <p className="font-medium text-sm">{customer?.name}</p>
                            {customer?.email && (
                                <p className="text-xs text-muted-foreground">{customer.email}</p>
                            )}
                            {customer?.phone && (
                                <p className="text-xs text-muted-foreground">{customer.phone}</p>
                            )}
                        </div>
                        <div className="flex gap-1">
                            <Button
                                variant="ghost"
                                size="sm"
                                onClick={() => setIsEditing(true)}
                                className="h-6 w-6 p-0"
                            >
                                <User className="h-3 w-3" />
                            </Button>
                            <Button
                                variant="ghost"
                                size="sm"
                                onClick={handleRemoveCustomer}
                                className="h-6 w-6 p-0 text-destructive"
                            >
                                <X className="h-3 w-3" />
                            </Button>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </div>
    );
}