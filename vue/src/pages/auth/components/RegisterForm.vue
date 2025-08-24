<template>
  <Card class="w-full max-w-md mx-auto">
    <CardHeader class="space-y-1">
      <CardTitle class="text-2xl font-bold text-center">Create account</CardTitle>
      <CardDescription class="text-center">
        Enter your details to create a new account
      </CardDescription>
    </CardHeader>
    <CardContent>
      <form @submit="onSubmit" class="space-y-4">
        <FormField v-slot="{ componentField, errorMessage }" name="name">
          <FormItem>
            <FormLabel>Full Name</FormLabel>
            <FormControl>
              <Input type="text" placeholder="Enter your full name" v-bind="componentField" />
            </FormControl>
            <FormDescription> Name must be between 3 and 255 characters </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>

        <FormField v-slot="{ componentField, errorMessage }" name="email">
          <FormItem>
            <FormLabel>Email</FormLabel>
            <FormControl>
              <Input type="email" placeholder="Enter your email" v-bind="componentField" />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <FormField v-slot="{ componentField, errorMessage }" name="phone">
          <FormItem>
            <FormLabel>Phone</FormLabel>
            <FormControl>
              <Input type="tel" placeholder="Enter your phone number" v-bind="componentField" />
            </FormControl>
            <FormDescription> Phone must be between 10 and 20 digits </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>

        <FormField v-slot="{ componentField, errorMessage }" name="password">
          <FormItem>
            <FormLabel>Password</FormLabel>
            <FormControl>
              <Input type="password" placeholder="Create a password" v-bind="componentField" />
            </FormControl>
            <FormDescription> Password must be between 8 and 32 characters </FormDescription>
            <FormMessage />
          </FormItem>
        </FormField>

        <FormField v-slot="{ componentField, errorMessage }" name="confirmPassword">
          <FormItem>
            <FormLabel>Confirm Password</FormLabel>
            <FormControl>
              <Input type="password" placeholder="Confirm your password" v-bind="componentField" />
            </FormControl>
            <FormMessage />
          </FormItem>
        </FormField>

        <Button type="submit" class="w-full" :disabled="loading">
          {{ loading ? "Creating account..." : "Create account" }}
        </Button>
      </form>
    </CardContent>
    <CardFooter class="flex justify-center">
      <p class="text-sm text-muted-foreground">
        Already have an account?
        <Button variant="link" @click="$emit('switchToLogin')" class="p-0 h-auto"> Sign in </Button>
      </p>
    </CardFooter>
  </Card>
</template>

<script setup lang="ts">
import { ref } from "vue";
import { useForm } from "vee-validate";
import { toTypedSchema } from "@vee-validate/zod";
import * as z from "zod";
import { toast } from "vue-sonner";
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useAuth } from "@/composables/useAuth";
import { useRouter } from "vue-router";
interface RegisterForm {
  name: string;
  email: string;
  phone: string;
  password: string;
  confirmPassword: string;
}

const emit = defineEmits<{
  switchToLogin: [];
}>();

const { register } = useAuth();
const loading = ref(false);
const router = useRouter();
const formSchema = toTypedSchema(
  z
    .object({
      name: z
        .string()
        .min(3, "Name must be at least 3 characters")
        .max(255, "Name must be less than 255 characters"),
      email: z.string().email("Please enter a valid email address"),
      phone: z
        .string()
        .regex(/^[0-9]+$/, "Phone must contain only digits")
        .min(10, "Phone must be at least 10 digits")
        .max(20, "Phone must be less than 20 digits"),
      password: z
        .string()
        .min(8, "Password must be at least 8 characters")
        .max(32, "Password must be less than 32 characters"),
      confirmPassword: z.string(),
    })
    .refine((data) => data.password === data.confirmPassword, {
      message: "Passwords don't match",
      path: ["confirmPassword"],
    })
);

const form = useForm({
  validationSchema: formSchema,
});

const handleRegister = async (form: {
  name: string;
  email: string;
  phone: string;
  password: string;
}) => {
  const result = await register(form.name, form.email, form.phone, form.password);
  if (result.success) {
    console.log("Registration successful");
    toast.success("Account created successfully!");
    // Switch back to login form after successful registration
    router.push("/organization-selection");
    emit("switchToLogin");
  } else {
    // Show error toast
    toast.error(result.error || "Registration failed");
  }
};

const onSubmit = form.handleSubmit(async (values) => {
  loading.value = true;
  try {
    // Remove confirmPassword before sending to backend
    const { confirmPassword, ...registerData } = values;
    await handleRegister(registerData);
  } finally {
    loading.value = false;
  }
});
</script>
