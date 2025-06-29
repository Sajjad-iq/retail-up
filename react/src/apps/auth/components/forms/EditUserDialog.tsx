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
import { Eye, EyeOff, AlertCircle, UserCheck, Loader2 } from 'lucide-react';

import { useUsers, useRoles } from '../../hooks/use-auth';
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
 * Includes form validation, role selection, and optional password reset.
 */
export function EditUserDialog({ isOpen, user, onClose, onSuccess }: EditUserDialogProps) {
    const [showPassword, setShowPassword] = useState(false);
    const [showPasswordField, setShowPasswordField] = useState(false);
    const [submitError, setSubmitError] = useState<string>('');

    const { updateUser, loading } = useUsers();
    const { roles } = useRoles();

    const form = useForm<EditUserFormInput>({
        resolver: zodResolver(editUserFormSchema),
        defaultValues: {
            name: '',
            email: '',
            phone: '',
            department: '',
            employeeId: '',
            roleId: '',
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
                roleId: user.role.id,
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

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <UserCheck className="h-5 w-5" />
                        Edit User: {user.name}
                    </DialogTitle>
                    <DialogDescription>
                        Update user information and role-based permissions.
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

                        {/* Role and Status */}
                        <div className="space-y-4">
                            <h4 className="text-sm font-medium">Role and Access</h4>

                            <div className="grid grid-cols-2 gap-4">
                                <FormField
                                    control={form.control}
                                    name="roleId"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Role *</FormLabel>
                                            <Select
                                                onValueChange={field.onChange}
                                                value={field.value}
                                            >
                                                <FormControl>
                                                    <SelectTrigger>
                                                        <SelectValue placeholder="Select a role" />
                                                    </SelectTrigger>
                                                </FormControl>
                                                <SelectContent>
                                                    {roles.map(role => (
                                                        <SelectItem key={role.id} value={role.id}>
                                                            <div className="flex items-center gap-2">
                                                                <Badge
                                                                    variant="outline"
                                                                    className="text-xs"
                                                                    style={{
                                                                        borderColor: role.color || '#6B7280',
                                                                        color: role.color || '#6B7280'
                                                                    }}
                                                                >
                                                                    {role.name}
                                                                </Badge>
                                                                {role.description && (
                                                                    <span className="text-sm text-muted-foreground">
                                                                        - {role.description}
                                                                    </span>
                                                                )}
                                                            </div>
                                                        </SelectItem>
                                                    ))}
                                                </SelectContent>
                                            </Select>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />

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
                        </div>

                        {/* Password */}
                        <div className="space-y-4">
                            <div className="flex items-center justify-between">
                                <h4 className="text-sm font-medium">Password</h4>
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