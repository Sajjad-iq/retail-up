import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
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
import { Switch } from '@/components/ui/switch';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Eye, EyeOff, AlertCircle, UserCheck, Loader2, Shield, Check } from 'lucide-react';

import { useUsers } from '../../hooks/use-auth';
import { useAuthStore } from '../../store/auth-store';
import { groupPermissionsByCategory } from '../../lib/utils/auth-utils';
import { editUserFormSchema, type EditUserFormInput } from '../../lib/validations/auth-schemas';
import { generateSecurePassword } from '../../lib/utils/auth-utils';
import type { User } from '../../types/auth';

/**
 * EditUserDialog component props
 */
interface EditUserDialogProps {
    /** Whether the dialog is open */
    isOpen: boolean;
    /** User to edit */
    user: User | null;
    /** Callback when dialog should close */
    onClose: () => void;
    /** Callback when user is successfully updated */
    onSuccess?: () => void;
}

/**
 * EditUserDialog Component
 * 
 * Dialog form for editing existing users.
 * Includes form validation, role selection, permissions management, and optional password reset.
 */
export function EditUserDialog({ isOpen, user, onClose, onSuccess }: EditUserDialogProps) {
    const [showPassword, setShowPassword] = useState(false);
    const [showPasswordField, setShowPasswordField] = useState(false);
    const [submitError, setSubmitError] = useState<string>('');

    const { updateUser, loading } = useUsers();
    const { permissions } = useAuthStore();
    const groupedPermissions = groupPermissionsByCategory(permissions);

    const form = useForm<EditUserFormInput>({
        resolver: zodResolver(editUserFormSchema),
        defaultValues: {
            name: '',
            email: '',
            phone: '',
            department: '',
            employeeId: '',
            permissionIds: [],
            status: 'active',
            password: '',
            mustChangePassword: false,
        },
    });

    // Reset form when user changes
    useEffect(() => {
        if (user && isOpen) {
            form.reset({
                name: user.name,
                email: user.email,
                phone: user.phone || '',
                department: user.department || '',
                employeeId: user.employeeId || '',
                permissionIds: user.permissions?.map(p => p.id) || [],
                status: user.status,
                password: '',
                mustChangePassword: user.mustChangePassword || false,
            });
            setShowPasswordField(false);
            setShowPassword(false);
            setSubmitError('');
        }
    }, [user, isOpen, form]);

    /**
     * Generate a secure password
     */
    const handleGeneratePassword = () => {
        const password = generateSecurePassword(12);
        form.setValue('password', password);
        form.clearErrors('password');
    };

    /**
     * Toggle password field visibility
     */
    const handleTogglePasswordField = () => {
        setShowPasswordField(!showPasswordField);
        if (!showPasswordField) {
            form.setValue('password', '');
            form.setValue('mustChangePassword', true);
        }
    };

    /**
     * Handle permission toggle
     */
    const handlePermissionToggle = (permissionId: string) => {
        const currentPermissions = form.getValues('permissionIds');
        const newPermissions = currentPermissions.includes(permissionId)
            ? currentPermissions.filter(id => id !== permissionId)
            : [...currentPermissions, permissionId];

        form.setValue('permissionIds', newPermissions);
    };

    /**
     * Handle form submission
     */
    const onSubmit = async (data: EditUserFormInput) => {
        if (!user) return;

        setSubmitError('');

        try {
            // Only include password if it's being changed
            const updateData = showPasswordField && data.password
                ? data
                : { ...data, password: undefined };

            const result = await updateUser(user.id, updateData);

            if (result.success) {
                onSuccess?.();
                onClose();
            } else {
                setSubmitError(result.error || 'Failed to update user');
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
            setShowPasswordField(false);
            setShowPassword(false);
            onClose();
        }
    };

    if (!user) {
        return null;
    }

    const selectedPermissions = form.watch('permissionIds');

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="max-w-4xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <UserCheck className="h-5 w-5" />
                        Edit User: {user.name}
                    </DialogTitle>
                    <DialogDescription>
                        Update user information and custom permissions.
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

                        <Tabs defaultValue="basic" className="w-full">
                            <TabsList className="grid w-full grid-cols-3">
                                <TabsTrigger value="basic">Basic Info</TabsTrigger>
                                <TabsTrigger value="permissions">Permissions</TabsTrigger>
                                <TabsTrigger value="security">Security</TabsTrigger>
                            </TabsList>

                            {/* Basic Information Tab */}
                            <TabsContent value="basic" className="space-y-6">
                                <div className="space-y-4">
                                    <h4 className="text-sm font-medium">Personal Information</h4>

                                    <div className="grid grid-cols-2 gap-4">
                                        <FormField
                                            control={form.control}
                                            name="name"
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Full Name *</FormLabel>
                                                    <FormControl>
                                                        <Input
                                                            placeholder="Enter full name"
                                                            {...field}
                                                        />
                                                    </FormControl>
                                                    <FormMessage />
                                                </FormItem>
                                            )}
                                        />

                                        <FormField
                                            control={form.control}
                                            name="email"
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Email Address *</FormLabel>
                                                    <FormControl>
                                                        <Input
                                                            type="email"
                                                            placeholder="Enter email address"
                                                            {...field}
                                                        />
                                                    </FormControl>
                                                    <FormMessage />
                                                </FormItem>
                                            )}
                                        />
                                    </div>

                                    <div className="grid grid-cols-2 gap-4">
                                        <FormField
                                            control={form.control}
                                            name="phone"
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Phone Number</FormLabel>
                                                    <FormControl>
                                                        <Input
                                                            placeholder="Enter phone number"
                                                            {...field}
                                                        />
                                                    </FormControl>
                                                    <FormMessage />
                                                </FormItem>
                                            )}
                                        />

                                        <FormField
                                            control={form.control}
                                            name="employeeId"
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Employee ID</FormLabel>
                                                    <FormControl>
                                                        <Input
                                                            placeholder="Enter employee ID"
                                                            {...field}
                                                        />
                                                    </FormControl>
                                                    <FormMessage />
                                                </FormItem>
                                            )}
                                        />
                                    </div>

                                    <FormField
                                        control={form.control}
                                        name="department"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Department</FormLabel>
                                                <FormControl>
                                                    <Input
                                                        placeholder="Enter department"
                                                        {...field}
                                                    />
                                                </FormControl>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />
                                </div>

                                <div className="space-y-4">
                                    <h4 className="text-sm font-medium">Status</h4>

                                    <FormField
                                        control={form.control}
                                        name="status"
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Status *</FormLabel>
                                                <Select
                                                    onValueChange={field.onChange}
                                                    value={field.value}
                                                >
                                                    <FormControl>
                                                        <SelectTrigger>
                                                            <SelectValue />
                                                        </SelectTrigger>
                                                    </FormControl>
                                                    <SelectContent>
                                                        <SelectItem value="active">Active</SelectItem>
                                                        <SelectItem value="inactive">Inactive</SelectItem>
                                                        <SelectItem value="pending">Pending</SelectItem>
                                                        <SelectItem value="suspended">Suspended</SelectItem>
                                                    </SelectContent>
                                                </Select>
                                                <FormMessage />
                                            </FormItem>
                                        )}
                                    />
                                </div>
                            </TabsContent>

                            {/* Permissions Tab */}
                            <TabsContent value="permissions" className="space-y-6">
                                <div className="space-y-4">
                                    <div className="flex items-center justify-between">
                                        <h4 className="text-sm font-medium flex items-center gap-2">
                                            <Shield className="h-4 w-4" />
                                            User Permissions
                                        </h4>
                                        <Badge variant="secondary">
                                            {selectedPermissions.length} permissions selected
                                        </Badge>
                                    </div>

                                    <p className="text-sm text-muted-foreground">
                                        Select permissions to grant to this user.
                                    </p>

                                    <ScrollArea className="h-[400px] w-full border rounded-md p-4">
                                        <div className="space-y-6">
                                            {Object.entries(groupedPermissions || {}).map(([category, categoryPermissions]) => (
                                                <div key={category} className="space-y-3">
                                                    <h5 className="text-sm font-medium text-primary capitalize">
                                                        {category} Permissions
                                                    </h5>
                                                    <div className="grid grid-cols-1 gap-3">
                                                        {categoryPermissions.map((permission) => (
                                                            <div
                                                                key={permission.id}
                                                                className="flex items-center space-x-3 p-3 border rounded-lg hover:bg-accent cursor-pointer"
                                                                onClick={() => handlePermissionToggle(permission.id)}
                                                            >
                                                                <div className="flex h-4 w-4 items-center justify-center rounded border">
                                                                    {selectedPermissions.includes(permission.id) && (
                                                                        <Check className="h-3 w-3 text-primary" />
                                                                    )}
                                                                </div>
                                                                <div className="flex-1 space-y-1">
                                                                    <div className="flex items-center gap-2">
                                                                        <span className="text-sm font-medium">
                                                                            {permission.label}
                                                                        </span>
                                                                        {permission.isSystem && (
                                                                            <Badge variant="outline" className="text-xs">
                                                                                System
                                                                            </Badge>
                                                                        )}
                                                                    </div>
                                                                    <p className="text-xs text-muted-foreground">
                                                                        {permission.description}
                                                                    </p>
                                                                </div>
                                                            </div>
                                                        ))}
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    </ScrollArea>
                                </div>
                            </TabsContent>

                            {/* Security Tab */}
                            <TabsContent value="security" className="space-y-6">
                                <div className="space-y-4">
                                    <div className="flex items-center justify-between">
                                        <h4 className="text-sm font-medium">Password Settings</h4>
                                        <Button
                                            type="button"
                                            variant="outline"
                                            size="sm"
                                            onClick={handleTogglePasswordField}
                                        >
                                            {showPasswordField ? 'Cancel Password Change' : 'Change Password'}
                                        </Button>
                                    </div>

                                    {showPasswordField && (
                                        <>
                                            <FormField
                                                control={form.control}
                                                name="password"
                                                render={({ field }) => (
                                                    <FormItem>
                                                        <FormLabel>New Password</FormLabel>
                                                        <div className="flex gap-2">
                                                            <FormControl>
                                                                <div className="relative flex-1">
                                                                    <Input
                                                                        type={showPassword ? 'text' : 'password'}
                                                                        placeholder="Enter new password"
                                                                        {...field}
                                                                    />
                                                                    <Button
                                                                        type="button"
                                                                        variant="ghost"
                                                                        size="sm"
                                                                        className="absolute right-0 top-0 h-full px-3 py-2 hover:bg-transparent"
                                                                        onClick={() => setShowPassword(!showPassword)}
                                                                    >
                                                                        {showPassword ? (
                                                                            <EyeOff className="h-4 w-4" />
                                                                        ) : (
                                                                            <Eye className="h-4 w-4" />
                                                                        )}
                                                                    </Button>
                                                                </div>
                                                            </FormControl>
                                                            <Button
                                                                type="button"
                                                                variant="outline"
                                                                onClick={handleGeneratePassword}
                                                            >
                                                                Generate
                                                            </Button>
                                                        </div>
                                                        <FormMessage />
                                                    </FormItem>
                                                )}
                                            />

                                            <FormField
                                                control={form.control}
                                                name="mustChangePassword"
                                                render={({ field }) => (
                                                    <FormItem className="flex flex-row items-center space-x-2 space-y-0">
                                                        <FormControl>
                                                            <Switch
                                                                checked={field.value}
                                                                onCheckedChange={field.onChange}
                                                            />
                                                        </FormControl>
                                                        <FormLabel className="text-sm font-normal">
                                                            Require password change on next login
                                                        </FormLabel>
                                                    </FormItem>
                                                )}
                                            />
                                        </>
                                    )}
                                </div>
                            </TabsContent>
                        </Tabs>

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
                                        Updating User...
                                    </>
                                ) : (
                                    <>
                                        <UserCheck className="mr-2 h-4 w-4" />
                                        Update User
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