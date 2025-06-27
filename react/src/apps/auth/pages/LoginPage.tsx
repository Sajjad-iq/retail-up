import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState } from 'react';
import { Eye, EyeOff, LogIn, Loader2, Shield, AlertCircle } from 'lucide-react';

import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import {
    Form,
    FormControl,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from '@/components/ui/form';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Switch } from '@/components/ui/switch';

import { useAuth } from '../hooks/use-auth';
import { loginCredentialsSchema, type LoginCredentialsInput } from '../lib/validations/auth-schemas';

/**
 * LoginPage component props
 */
interface LoginPageProps {
    /** Callback when login is successful */
    onSuccess?: () => void;
    /** Additional CSS classes */
    className?: string;
}

/**
 * LoginPage Component
 * 
 * Authentication login page with form validation and error handling.
 * Follows the established auth app patterns and design system.
 */
export function LoginPage({ onSuccess, className }: LoginPageProps) {
    const [showPassword, setShowPassword] = useState(false);
    const [submitError, setSubmitError] = useState<string>('');

    const { login, isLoading, authError } = useAuth();

    const form = useForm<LoginCredentialsInput>({
        resolver: zodResolver(loginCredentialsSchema),
        defaultValues: {
            email: '',
            password: '',
            rememberMe: false,
        },
    });

    /**
     * Handle form submission
     */
    const onSubmit = async (data: LoginCredentialsInput) => {
        setSubmitError('');

        try {
            const result = await login(data);

            if (result.success) {
                form.reset();
                onSuccess?.();
            } else {
                setSubmitError(result.error || 'Login failed. Please check your credentials.');
            }
        } catch (error) {
            setSubmitError('An unexpected error occurred. Please try again.');
        }
    };

    /**
     * Toggle password visibility
     */
    const togglePasswordVisibility = () => {
        setShowPassword(!showPassword);
    };

    return (
        <div className={`min-h-screen flex items-center justify-center bg-background p-4 ${className || ''}`}>
            <div className="w-full max-w-md">
                <Card className="shadow-lg">
                    <CardHeader className="space-y-4 text-center">
                        <div className="mx-auto w-12 h-12 rounded-full bg-primary/10 flex items-center justify-center">
                            <Shield className="h-6 w-6 text-primary" />
                        </div>
                        <div>
                            <CardTitle className="text-2xl font-bold">Welcome Back</CardTitle>
                            <CardDescription>
                                Sign in to your account to access the retail management system
                            </CardDescription>
                        </div>
                    </CardHeader>

                    <CardContent>
                        <Form {...form}>
                            <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-6">
                                {/* Authentication Error */}
                                {(submitError || authError) && (
                                    <Alert variant="destructive">
                                        <AlertCircle className="h-4 w-4" />
                                        <AlertDescription>
                                            {submitError || authError}
                                        </AlertDescription>
                                    </Alert>
                                )}

                                {/* Email Field */}
                                <FormField
                                    control={form.control}
                                    name="email"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Email Address</FormLabel>
                                            <FormControl>
                                                <Input
                                                    type="email"
                                                    placeholder="Enter your email address"
                                                    disabled={isLoading}
                                                    {...field}
                                                />
                                            </FormControl>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />

                                {/* Password Field */}
                                <FormField
                                    control={form.control}
                                    name="password"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Password</FormLabel>
                                            <FormControl>
                                                <div className="relative">
                                                    <Input
                                                        type={showPassword ? 'text' : 'password'}
                                                        placeholder="Enter your password"
                                                        disabled={isLoading}
                                                        {...field}
                                                    />
                                                    <Button
                                                        type="button"
                                                        variant="ghost"
                                                        size="sm"
                                                        className="absolute right-0 top-0 h-full px-3 py-2 hover:bg-transparent"
                                                        onClick={togglePasswordVisibility}
                                                        disabled={isLoading}
                                                    >
                                                        {showPassword ? (
                                                            <EyeOff className="h-4 w-4 text-muted-foreground" />
                                                        ) : (
                                                            <Eye className="h-4 w-4 text-muted-foreground" />
                                                        )}
                                                    </Button>
                                                </div>
                                            </FormControl>
                                            <FormMessage />
                                        </FormItem>
                                    )}
                                />

                                {/* Remember Me */}
                                <FormField
                                    control={form.control}
                                    name="rememberMe"
                                    render={({ field }) => (
                                        <FormItem className="flex flex-row items-center justify-between rounded-lg border p-3">
                                            <div className="space-y-0.5">
                                                <FormLabel className="text-sm font-medium">
                                                    Remember me
                                                </FormLabel>
                                                <div className="text-xs text-muted-foreground">
                                                    Keep me signed in for 30 days
                                                </div>
                                            </div>
                                            <FormControl>
                                                <Switch
                                                    checked={field.value}
                                                    onCheckedChange={field.onChange}
                                                    disabled={isLoading}
                                                />
                                            </FormControl>
                                        </FormItem>
                                    )}
                                />

                                {/* Submit Button */}
                                <Button
                                    type="submit"
                                    className="w-full"
                                    disabled={isLoading}
                                >
                                    {isLoading ? (
                                        <>
                                            <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                            Signing in...
                                        </>
                                    ) : (
                                        <>
                                            <LogIn className="mr-2 h-4 w-4" />
                                            Sign In
                                        </>
                                    )}
                                </Button>
                            </form>
                        </Form>

                        {/* Additional Options */}
                        <div className="mt-6 text-center">
                            <Button variant="link" className="text-sm text-muted-foreground hover:text-primary">
                                Forgot your password?
                            </Button>
                        </div>
                    </CardContent>
                </Card>

                {/* Footer */}
                <div className="mt-8 text-center text-xs text-muted-foreground">
                    <p>© 2024 Retail Management System. All rights reserved.</p>
                </div>
            </div>
        </div>
    );
} 