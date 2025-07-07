import { z } from 'zod';

/**
 * Helper function to validate password strength
 */
const isStrongPassword = (password: string): boolean => {
    const hasMinLength = password.length >= 8;
    const hasUpperCase = /[A-Z]/.test(password);
    const hasLowerCase = /[a-z]/.test(password);
    const hasNumber = /\d/.test(password);
    const hasSpecialChar = /[!@#$%^&*(),.?":{}|<>]/.test(password);

    return hasMinLength && hasUpperCase && hasLowerCase && hasNumber && hasSpecialChar;
};

/**
 * Helper function to validate phone numbers
 */
const isValidPhone = (phone: string): boolean => {
    const phoneRegex = /^\+?[\d\s\-\(\)]{10,}$/;
    return phoneRegex.test(phone);
};



/**
 * Login credentials validation schema
 */
export const loginCredentialsSchema = z.object({
    email: z.string()
        .min(1, 'Email is required')
        .email('Please enter a valid email address'),
    password: z.string()
        .min(6, 'Password must be at least 6 characters'),
    rememberMe: z.boolean().optional(),
});

/**
 * User form validation schema
 */
export const userFormSchema = z.object({
    name: z.string()
        .min(1, 'Name is required')
        .min(2, 'Name must be at least 2 characters')
        .max(50, 'Name must not exceed 50 characters')
        .transform(val => val.trim()),
    email: z.string()
        .min(1, 'Email is required')
        .email('Please enter a valid email address'),
    phone: z.string()
        .optional()
        .refine((phone) => !phone || isValidPhone(phone), {
            message: 'Please enter a valid phone number'
        }),
    department: z.string().optional(),
    employeeId: z.string()
        .optional()
        .refine((id) => !id || (id.length >= 2 && id.length <= 20), {
            message: 'Employee ID must be between 2 and 20 characters'
        }),
    permissionIds: z.array(z.string()).default([]),
    status: z.enum(['active', 'inactive', 'suspended', 'pending'] as const, {
        errorMap: () => ({ message: 'Invalid status value' }),
    }),
    password: z.string()
        .optional()
        .refine((password) => !password || password.length >= 8, {
            message: 'Password must be at least 8 characters'
        })
        .refine((password) => !password || isStrongPassword(password), {
            message: 'Password must contain uppercase, lowercase, number, and special character'
        }),
    mustChangePassword: z.boolean().optional(),
});

/**
 * User form schema for creation (password required)
 */
export const createUserFormSchema = z.object({
    name: z.string()
        .min(1, 'Name is required')
        .min(2, 'Name must be at least 2 characters')
        .max(50, 'Name must not exceed 50 characters')
        .transform(val => val.trim()),
    email: z.string()
        .min(1, 'Email is required')
        .email('Please enter a valid email address'),
    phone: z.string()
        .optional()
        .refine((phone) => !phone || isValidPhone(phone), {
            message: 'Please enter a valid phone number'
        }),
    department: z.string().optional(),
    employeeId: z.string()
        .optional()
        .refine((id) => !id || (id.length >= 2 && id.length <= 20), {
            message: 'Employee ID must be between 2 and 20 characters'
        }),
    permissionIds: z.array(z.string()),
    status: z.enum(['active', 'inactive', 'suspended', 'pending'] as const, {
        required_error: 'Status is required'
    }),
    password: z.string()
        .min(8, 'Password must be at least 8 characters')
        .refine(isStrongPassword, {
            message: 'Password must contain uppercase, lowercase, number, and special character'
        }),
    mustChangePassword: z.boolean(),
});

/**
 * User form schema for editing (password optional)
 */
export const editUserFormSchema = userFormSchema.extend({
    permissionIds: z.array(z.string()),
    password: z.string().optional(),
});



/**
 * Password change validation schema
 */
export const passwordChangeSchema = z.object({
    currentPassword: z.string().min(1, 'Current password is required'),
    newPassword: z.string()
        .min(8, 'New password must be at least 8 characters')
        .refine(isStrongPassword, {
            message: 'Password must contain uppercase, lowercase, number, and special character'
        }),
    confirmPassword: z.string().min(1, 'Please confirm your new password'),
}).refine((data) => data.newPassword === data.confirmPassword, {
    message: 'Passwords do not match',
    path: ['confirmPassword'],
}).refine((data) => data.currentPassword !== data.newPassword, {
    message: 'New password must be different from current password',
    path: ['newPassword'],
});

/**
 * Search and filter schema
 */
export const userSearchSchema = z.object({
    query: z.string().max(100, 'Search query too long').optional(),
    roleId: z.string().optional(),
    status: z.enum(['active', 'inactive', 'suspended', 'pending'] as const).optional(),
    department: z.string().optional(),
});

/**
 * Bulk user operations schema
 */
export const bulkUserOperationSchema = z.object({
    userIds: z.array(z.string()).min(1, 'At least one user must be selected'),
    operation: z.enum(['activate', 'deactivate', 'suspend', 'delete']),
});

// Type exports
export type LoginCredentialsInput = z.infer<typeof loginCredentialsSchema>;
export type UserFormInput = z.infer<typeof userFormSchema>;
export type CreateUserFormInput = z.infer<typeof createUserFormSchema>;
export type EditUserFormInput = z.infer<typeof editUserFormSchema>;

export type PasswordChangeInput = z.infer<typeof passwordChangeSchema>;
export type UserSearchInput = z.infer<typeof userSearchSchema>;
export type BulkUserOperationInput = z.infer<typeof bulkUserOperationSchema>;

// Legacy function exports for backward compatibility
export const validateUserFormData = (userData: any, isEditing: boolean = false) => {
    const schema = isEditing ? editUserFormSchema : createUserFormSchema;
    const result = schema.safeParse(userData);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
};

export const validateLoginCredentials = (credentials: any) => {
    const result = loginCredentialsSchema.safeParse(credentials);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
};



export const validatePasswordChangeFormData = (passwordData: any) => {
    const result = passwordChangeSchema.safeParse(passwordData);

    if (result.success) {
        return { isValid: true, errors: {} };
    }

    const errors: Record<string, string> = {};
    result.error.errors.forEach((error) => {
        const path = error.path.join('.');
        errors[path] = error.message;
    });

    return { isValid: false, errors };
};