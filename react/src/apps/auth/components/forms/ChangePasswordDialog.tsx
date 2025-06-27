import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState, useEffect } from 'react';
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
import { Switch } from '@/components/ui/switch';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Eye, EyeOff, AlertCircle, Lock, Loader2 } from 'lucide-react';

import { useUsers } from '../../hooks/use-auth';
import { passwordChangeSchema, type PasswordChangeInput } from '../../lib/validations/auth-schemas';
import { generateSecurePassword } from '../../lib/utils/auth-utils';
import type { User } from '../../types/auth';

/**
 * ChangePasswordDialog component props
 */
interface ChangePasswordDialogProps {
    /** Whether the dialog is open */
    isOpen: boolean;
    /** User to change password for */
    user: User | null;
    /** Callback when dialog should close */
    onClose: () => void;
    /** Callback when password is successfully changed */
    onSuccess?: () => void;
}

/**
 * ChangePasswordDialog Component
 * 
 * Dialog form for changing user passwords with validation.
 * Includes current password verification and new password confirmation.
 */
export function ChangePasswordDialog({ isOpen, user, onClose, onSuccess }: ChangePasswordDialogProps) {
    const [showCurrentPassword, setShowCurrentPassword] = useState(false);
    const [showNewPassword, setShowNewPassword] = useState(false);
    const [showConfirmPassword, setShowConfirmPassword] = useState(false);
    const [submitError, setSubmitError] = useState<string>('');

    const { changePassword, loading } = useUsers();

    const form = useForm<PasswordChangeInput>({
        resolver: zodResolver(passwordChangeSchema),
        defaultValues: {
            currentPassword: '',
            newPassword: '',
            confirmPassword: '',
        },
    });

    // Reset form when user changes
    useEffect(() => {
        if (user && isOpen) {
            form.reset({
                currentPassword: '',
                newPassword: '',
                confirmPassword: '',
            });
            setSubmitError('');
            setShowCurrentPassword(false);
            setShowNewPassword(false);
            setShowConfirmPassword(false);
        }
    }, [user, isOpen, form]);

    /**
     * Generate a secure password
     */
    const handleGeneratePassword = () => {
        const password = generateSecurePassword(12);
        form.setValue('newPassword', password);
        form.setValue('confirmPassword', password);
        form.clearErrors(['newPassword', 'confirmPassword']);
    };

    /**
     * Handle form submission
     */
    const onSubmit = async (data: PasswordChangeInput) => {
        if (!user) return;

        setSubmitError('');

        try {
            const result = await changePassword(user.id, data);

            if (result.success) {
                form.reset();
                onSuccess?.();
                onClose();
            } else {
                setSubmitError(result.error || 'Failed to change password');
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
            setShowCurrentPassword(false);
            setShowNewPassword(false);
            setShowConfirmPassword(false);
            onClose();
        }
    };

    if (!user) {
        return null;
    }

    return (
        <Dialog open={isOpen} onOpenChange={handleClose}>
            <DialogContent className="max-w-md">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Lock className="h-5 w-5" />
                        Change Password
                    </DialogTitle>
                    <DialogDescription>
                        Change password for {user.name}
                    </DialogDescription>
                </DialogHeader>

                <Form {...form}>
                    <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
                        {/* General Error */}
                        {submitError && (
                            <Alert variant="destructive">
                                <AlertCircle className="h-4 w-4" />
                                <AlertDescription>{submitError}</AlertDescription>
                            </Alert>
                        )}

                        {/* Current Password */}
                        <FormField
                            control={form.control}
                            name="currentPassword"
                            render={({ field }) => (
                                <FormItem>
                                    <FormLabel>Current Password *</FormLabel>
                                    <FormControl>
                                        <div className="relative">
                                            <Input
                                                type={showCurrentPassword ? 'text' : 'password'}
                                                placeholder="Enter current password"
                                                {...field}
                                            />
                                            <Button
                                                type="button"
                                                variant="ghost"
                                                size="sm"
                                                className="absolute right-0 top-0 h-full px-3 py-2 hover:bg-transparent"
                                                onClick={() => setShowCurrentPassword(!showCurrentPassword)}
                                            >
                                                {showCurrentPassword ? (
                                                    <EyeOff className="h-4 w-4" />
                                                ) : (
                                                    <Eye className="h-4 w-4" />
                                                )}
                                            </Button>
                                        </div>
                                    </FormControl>
                                    <FormMessage />
                                </FormItem>
                            )}
                        />

                        {/* New Password */}
                        <FormField
                            control={form.control}
                            name="newPassword"
                            render={({ field }) => (
                                <FormItem>
                                    <FormLabel>New Password *</FormLabel>
                                    <div className="flex gap-2">
                                        <FormControl>
                                            <div className="relative flex-1">
                                                <Input
                                                    type={showNewPassword ? 'text' : 'password'}
                                                    placeholder="Enter new password"
                                                    {...field}
                                                />
                                                <Button
                                                    type="button"
                                                    variant="ghost"
                                                    size="sm"
                                                    className="absolute right-0 top-0 h-full px-3 py-2 hover:bg-transparent"
                                                    onClick={() => setShowNewPassword(!showNewPassword)}
                                                >
                                                    {showNewPassword ? (
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

                        {/* Confirm Password */}
                        <FormField
                            control={form.control}
                            name="confirmPassword"
                            render={({ field }) => (
                                <FormItem>
                                    <FormLabel>Confirm New Password *</FormLabel>
                                    <FormControl>
                                        <div className="relative">
                                            <Input
                                                type={showConfirmPassword ? 'text' : 'password'}
                                                placeholder="Confirm new password"
                                                {...field}
                                            />
                                            <Button
                                                type="button"
                                                variant="ghost"
                                                size="sm"
                                                className="absolute right-0 top-0 h-full px-3 py-2 hover:bg-transparent"
                                                onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                                            >
                                                {showConfirmPassword ? (
                                                    <EyeOff className="h-4 w-4" />
                                                ) : (
                                                    <Eye className="h-4 w-4" />
                                                )}
                                            </Button>
                                        </div>
                                    </FormControl>
                                    <FormMessage />
                                </FormItem>
                            )}
                        />

                        {/* Password Requirements */}
                        <div className="text-xs text-muted-foreground space-y-1">
                            <p className="font-medium">Password must contain:</p>
                            <ul className="list-disc list-inside space-y-0.5 ml-2">
                                <li>At least 8 characters</li>
                                <li>At least one uppercase letter</li>
                                <li>At least one lowercase letter</li>
                                <li>At least one number</li>
                                <li>At least one special character</li>
                            </ul>
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
                                        Changing Password...
                                    </>
                                ) : (
                                    <>
                                        <Lock className="mr-2 h-4 w-4" />
                                        Change Password
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