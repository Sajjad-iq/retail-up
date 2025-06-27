import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import {
    Form,
    FormControl,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from '@/components/ui/form';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { AlertCircle, Shield, Loader2 } from 'lucide-react';

import { useRoles } from '../../hooks/use-auth';
import { roleFormSchema, type RoleFormInput } from '../../lib/validations/auth-schemas';
import type { Permission } from '../../types/auth';

/**
 * AddRoleDialog component props
 */
interface AddRoleDialogProps {
    /** Whether the dialog is open */
    isOpen: boolean;
    /** Callback when dialog should close */
    onClose: () => void;
    /** Callback when role is successfully added */
    onSuccess?: () => void;
}

/**
 * AddRoleDialog Component
 * 
 * Dialog form for adding new roles with permission selection.
 * Includes form validation and permission categorization.
 */
export function AddRoleDialog({ isOpen, onClose, onSuccess }: AddRoleDialogProps) {
    const [submitError, setSubmitError] = useState<string>('');

    const { addRole, loading, permissions } = useRoles();

    const form = useForm<RoleFormInput>({
        resolver: zodResolver(roleFormSchema),
        defaultValues: {
            name: '',
            description: '',
            permissionIds: [],
            color: '#3B82F6',
        },
    });

    // Group permissions by category
    const groupedPermissions = permissions.reduce((acc, permission) => {
        if (!acc[permission.category]) {
            acc[permission.category] = [];
        }
        acc[permission.category].push(permission);
        return acc;
    }, {} as Record<string, Permission[]>);

    /**
     * Handle permission toggle
     */
    const handlePermissionToggle = (permissionId: string) => {
        const currentPermissions = form.getValues('permissionIds');
        const newPermissions = currentPermissions.includes(permissionId)
            ? currentPermissions.filter(id => id !== permissionId)
            : [...currentPermissions, permissionId];

        form.setValue('permissionIds', newPermissions);
        form.clearErrors('permissionIds');
    };

    /**
     * Handle select all permissions for a category
     */
    const handleSelectAllCategory = (category: string) => {
        const categoryPermissions = groupedPermissions[category] || [];
        const categoryPermissionIds = categoryPermissions.map(p => p.id);
        const currentPermissions = form.getValues('permissionIds');

        // Check if all permissions in this category are already selected
        const allSelected = categoryPermissionIds.every(id => currentPermissions.includes(id));

        if (allSelected) {
            // Remove all permissions from this category
            const newPermissions = currentPermissions.filter(id => !categoryPermissionIds.includes(id));
            form.setValue('permissionIds', newPermissions);
        } else {
            // Add all permissions from this category
            const newPermissions = [...new Set([...currentPermissions, ...categoryPermissionIds])];
            form.setValue('permissionIds', newPermissions);
        }

        form.clearErrors('permissionIds');
    };

    /**
     * Handle form submission
     */
    const onSubmit = async (data: RoleFormInput) => {
        setSubmitError('');

        try {
            const result = await addRole(data);

            if (result.success) {
                form.reset();
                onSuccess?.();
                onClose();
            } else {
                setSubmitError(result.error || 'Failed to add role');
            }
        } catch (error) {
            setSubmitError('An unexpected error occurred');
        }
    };

    /**
     * Handle dialog close
     */
    const handleClose = () => {
        if (!form.formState.isSubmitting) {
            form.reset();
            setSubmitError('');
            onClose();
        }
    };

    const watchedPermissions = form.watch('permissionIds');

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="max-w-3xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Shield className="h-5 w-5" />
                        Add New Role
                    </DialogTitle>
                    <DialogDescription>
                        Create a new role with specific permissions for system access.
                    </DialogDescription>
                </DialogHeader>

                <Form {...form}>
                    <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-6">
                        {/* General Error */}
                        {submitError && (
                            <Alert variant="destructive">
                                <AlertCircle className="h-4 w-4" />
                                <AlertDescription>{submitError}</AlertDescription>
                            </Alert>
                        )}

                        {/* Basic Information */}
                        <div className="space-y-4">
                            <h4 className="text-sm font-medium">Basic Information</h4>

                            <div className="grid grid-cols-2 gap-4">
                                <FormField
                                    control={form.control}
                                    name="name"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Role Name *</FormLabel>
                                            <FormControl>
                                                <Input
                                                    placeholder="Enter role name"
                                                    {...field}
                                                />
                                            </FormControl>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />

                                <FormField
                                    control={form.control}
                                    name="color"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Color</FormLabel>
                                            <FormControl>
                                                <div className="flex gap-2 items-center">
                                                    <Input
                                                        type="color"
                                                        className="w-16 h-8 p-1"
                                                        {...field}
                                                    />
                                                    <Input
                                                        placeholder="#3B82F6"
                                                        className="flex-1"
                                                        {...field}
                                                    />
                                                </div>
                                            </FormControl>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />
                            </div>

                            <FormField
                                control={form.control}
                                name="description"
                                render={({ field }) => (
                                    <FormItem>
                                        <FormLabel>Description</FormLabel>
                                        <FormControl>
                                            <Input
                                                placeholder="Enter role description"
                                                {...field}
                                            />
                                        </FormControl>
                                        <FormMessage />
                                    </FormItem>
                                )}
                            />
                        </div>

                        {/* Permissions */}
                        <div className="space-y-4">
                            <div className="flex items-center justify-between">
                                <h4 className="text-sm font-medium">Permissions *</h4>
                                <Badge variant="outline">
                                    {watchedPermissions.length} selected
                                </Badge>
                            </div>

                            <FormField
                                control={form.control}
                                name="permissionIds"
                                render={() => (
                                    <FormItem>
                                        <div className="space-y-4">
                                            {Object.entries(groupedPermissions).map(([category, categoryPermissions]) => {
                                                const categoryPermissionIds = categoryPermissions.map(p => p.id);
                                                const selectedInCategory = categoryPermissionIds.filter(id => watchedPermissions.includes(id));
                                                const allSelected = categoryPermissionIds.length === selectedInCategory.length;

                                                return (
                                                    <div key={category} className="border rounded-lg p-4">
                                                        <div className="flex items-center justify-between mb-3">
                                                            <h5 className="font-medium capitalize">
                                                                {category}
                                                            </h5>
                                                            <div className="flex items-center gap-2">
                                                                <Badge variant="secondary" className="text-xs">
                                                                    {selectedInCategory.length}/{categoryPermissionIds.length}
                                                                </Badge>
                                                                <Button
                                                                    type="button"
                                                                    variant="outline"
                                                                    size="sm"
                                                                    onClick={() => handleSelectAllCategory(category)}
                                                                >
                                                                    {allSelected ? 'Deselect All' : 'Select All'}
                                                                </Button>
                                                            </div>
                                                        </div>

                                                        <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                                                            {categoryPermissions.map(permission => (
                                                                <div
                                                                    key={permission.id}
                                                                    className={`
                                                                        flex items-center gap-2 p-2 rounded cursor-pointer border transition-colors
                                                                        ${watchedPermissions.includes(permission.id)
                                                                            ? 'bg-blue-50 border-blue-200 dark:bg-blue-950 dark:border-blue-800'
                                                                            : 'hover:bg-gray-50 dark:hover:bg-gray-800'
                                                                        }
                                                                    `}
                                                                    onClick={() => handlePermissionToggle(permission.id)}
                                                                >
                                                                    <input
                                                                        type="checkbox"
                                                                        checked={watchedPermissions.includes(permission.id)}
                                                                        onChange={() => handlePermissionToggle(permission.id)}
                                                                        className="rounded"
                                                                    />
                                                                    <div>
                                                                        <div className="font-medium text-sm">
                                                                            {permission.name}
                                                                        </div>
                                                                        {permission.description && (
                                                                            <div className="text-xs text-muted-foreground">
                                                                                {permission.description}
                                                                            </div>
                                                                        )}
                                                                    </div>
                                                                </div>
                                                            ))}
                                                        </div>
                                                    </div>
                                                );
                                            })}
                                        </div>
                                        <FormMessage />
                                    </FormItem>
                                )}
                            />
                        </div>

                        <DialogFooter>
                            <Button
                                type="button"
                                variant="outline"
                                onClick={handleClose}
                                disabled={form.formState.isSubmitting}
                            >
                                Cancel
                            </Button>
                            <Button type="submit" disabled={form.formState.isSubmitting || loading}>
                                {form.formState.isSubmitting ? (
                                    <>
                                        <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                        Adding Role...
                                    </>
                                ) : (
                                    <>
                                        <Shield className="mr-2 h-4 w-4" />
                                        Add Role
                                    </>
                                )}
                            </Button>
                        </DialogFooter>
                    </form>
                </Form>
            </DialogContent>
        </Dialog>
    );
}