# Retail Up - Frontend

A modern retail management solution built with Vue 3, TypeScript, and shadcn/ui.

## Features

- **Authentication System**: User registration and login
- **Organization Management**: Create and manage retail organizations
- **Modern UI**: Built with shadcn/ui components and Tailwind CSS
- **TypeScript**: Full type safety throughout the application
- **State Management**: Pinia store for authentication and app state
- **Service Architecture**: Separated services for different domains

## Tech Stack

- Vue 3 with Composition API
- TypeScript
- Tailwind CSS
- shadcn/ui components
- Pinia for state management
- Axios for HTTP requests
- Vite for build tooling

## Project Structure

```
src/
├── components/
│   ├── auth/           # Authentication components
│   ├── organization/   # Organization management
│   ├── dashboard/      # Main dashboard
│   ├── layout/         # Layout components
│   └── ui/            # shadcn/ui components
├── config/             # Environment configuration
├── stores/             # Pinia stores
├── services/           # API services
│   ├── http.ts         # Base HTTP service (Axios)
│   ├── authService.ts  # Authentication service
│   └── organizationService.ts # Organization service
└── assets/             # Styles and assets
```

## Service Architecture

The application uses a clean service architecture:

### HTTP Service (`http.ts`)
- **Purpose**: Base Axios configuration and interceptors
- **Features**: 
  - Singleton pattern
  - Automatic token injection
  - Error handling (401 responses)
  - Request/response interceptors

### Authentication Service (`authService.ts`)
- **Purpose**: Handle all authentication-related API calls
- **Features**:
  - User login/register
  - Token management
  - Current user retrieval
  - Logout functionality

### Organization Service (`organizationService.ts`)
- **Purpose**: Handle all organization-related API calls
- **Features**:
  - Create/read/update/delete organizations
  - Organization listing
  - Business logic separation

## Environment Configuration

Create a `.env` file in the root directory:

```bash
# API Configuration
VITE_API_BASE_URL=http://localhost:8082/api

# App Configuration
VITE_APP_NAME=Retail Up
VITE_APP_VERSION=1.0.0
```

## API Endpoints

The frontend expects the following backend API endpoints:

### Authentication
- `POST /api/auth/register` - User registration
- `POST /api/auth/login` - User login
- `GET /api/auth/me` - Get current user (protected)
- `POST /api/auth/logout` - User logout (protected)

### Organizations
- `POST /api/organizations` - Create organization (protected)
- `GET /api/organizations` - List organizations (protected)
- `GET /api/organizations/{id}` - Get organization (protected)
- `PUT /api/organizations/{id}` - Update organization (protected)
- `DELETE /api/organizations/{id}` - Delete organization (protected)

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   # or
   pnpm install
   ```

2. Create environment file:
   ```bash
   cp .env.example .env
   # Edit .env with your backend URL
   ```

3. Start the development server:
   ```bash
   npm run dev
   # or
   pnpm dev
   ```

4. Open [http://localhost:5173](http://localhost:5173) in your browser.

## Backend Integration

Make sure your Java backend server is running and provides the required API endpoints. The API base URL can be configured in the `.env` file.

### CORS Configuration

Your backend should allow CORS from the frontend origin (typically `http://localhost:5173` for development).

## Build for Production

```bash
npm run build
# or
pnpm build
```

## Development

- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npm run lint` - Run ESLint
- `npm run type-check` - Run TypeScript type checking

## Service Benefits

- **Separation of Concerns**: Each service handles its own domain
- **Reusability**: Services can be used across different components
- **Testability**: Easy to mock and test individual services
- **Maintainability**: Clear structure and easy to extend
- **Type Safety**: Full TypeScript support for all service methods
