# React POS System

A modern, enterprise-level Point of Sale (POS) system built with React, TypeScript, and cutting-edge technologies.

## 🚀 Features

- **Modern UI**: Built with React 19 and shadcn/ui components
- **Type Safety**: Full TypeScript implementation with comprehensive type definitions
- **State Management**: Zustand for global state management with persistence
- **Form Validation**: Zod schema validation with react-hook-form
- **Routing**: React Router for navigation
- **Data Fetching**: TanStack Query for server state management
- **Testing**: Comprehensive test suite with Vitest
- **Enterprise Ready**: Clean architecture, JSDoc documentation, and professional code organization

## 🛠️ Tech Stack

- **Framework**: React 19 with TypeScript
- **UI Components**: shadcn/ui (Radix UI + Tailwind CSS)
- **State Management**: Zustand with persistence middleware
- **Form Handling**: React Hook Form with Zod validation
- **Routing**: React Router v7
- **Data Fetching**: TanStack Query (React Query)
- **Styling**: Tailwind CSS
- **Testing**: Vitest, React Testing Library, jsdom
- **Build Tool**: Vite
- **Package Manager**: pnpm

## 📁 Project Structure

```
src/
├── components/
│   ├── pos/                 # POS-specific components
│   │   ├── ProductGrid.tsx  # Product display and selection
│   │   ├── Cart.tsx         # Shopping cart management
│   │   ├── PaymentDialog.tsx # Payment processing
│   │   └── POSInterface.tsx # Main POS interface
│   └── ui/                  # shadcn/ui components
├── hooks/
│   └── use-pos.ts          # Custom hooks for POS logic
├── lib/
│   ├── utils/
│   │   └── pos-utils.ts    # Utility functions
│   └── validations/
│       └── pos-schemas.ts  # Zod validation schemas
├── store/
│   └── pos-store.ts        # Zustand store
├── types/
│   └── pos.ts              # TypeScript type definitions
└── tests/                  # Test files
    ├── setup.ts
    ├── pos-store.test.ts
    └── pos-utils.test.ts
```

## 🎯 Core Components

### ProductGrid
- Displays products in a responsive grid layout
- Category-based filtering
- Search functionality
- Stock status indicators
- Add to cart functionality

### Cart
- Real-time cart management
- Quantity adjustments
- Item removal
- Price calculations (subtotal, tax, total)
- Empty state handling

### PaymentDialog
- Multi-payment method support (cash, card, mobile)
- Form validation with Zod
- Payment processing simulation
- Receipt generation
- Success confirmation

### POSInterface
- Main dashboard combining all components
- Header with daily statistics
- Search functionality
- Recent transactions display
- Responsive layout

## 🔧 Installation & Setup

1. **Install dependencies**:
   ```bash
   pnpm install
   ```

2. **Run development server**:
   ```bash
   pnpm dev
   ```

3. **Run tests**:
   ```bash
   pnpm test        # Run tests in watch mode
   pnpm test:run    # Run tests once
   pnpm test:ui     # Run with UI
   pnpm test:coverage # Run with coverage
   ```

4. **Build for production**:
   ```bash
   pnpm build
   ```

## 🧪 Testing

The project includes comprehensive tests:

- **Store Tests**: Complete coverage of Zustand store operations
- **Utility Tests**: All utility functions tested
- **Component Tests**: React components with user interactions
- **Type Safety**: TypeScript ensures compile-time type checking

### Running Tests

```bash
# Run all tests in watch mode
pnpm test

# Run tests once
pnpm test:run

# Run with UI (requires @vitest/ui)
pnpm test:ui

# Run with coverage
pnpm test:coverage
```

## 📊 State Management

### Zustand Store Structure

```typescript
interface POSStore {
  // State
  products: Product[];
  cart: CartItem[];
  currentCustomer: Customer | null;
  transactions: Transaction[];
  
  // Actions
  addToCart: (product: Product, quantity?: number) => void;
  removeFromCart: (productId: string) => void;
  updateCartQuantity: (productId: string, quantity: number) => void;
  clearCart: () => void;
  processPayment: (paymentMethod: PaymentMethod) => Promise<Transaction>;
  
  // Computed
  getCartTotal: () => number;
  getCartSubtotal: () => number;
  getCartTax: () => number;
  getTodaysTransactions: () => Transaction[];
  getTodaysRevenue: () => number;
}
```

### Data Persistence

- Transaction history persisted to localStorage
- Customer data persisted to localStorage
- Cart state is session-only (cleared on refresh)

## 🎨 UI/UX Features

### Responsive Design
- Mobile-first approach
- Tablet and desktop optimized
- Touch-friendly interface

### Accessibility
- ARIA labels and descriptions
- Keyboard navigation support
- Screen reader compatible
- High contrast support

### Performance
- Lazy loading where appropriate
- Optimized re-renders with React.memo
- Efficient state updates
- Fast development with Vite HMR

## 🔐 Validation & Type Safety

### Form Validation
All forms use Zod schemas for validation:
- Payment form validation
- Product data validation
- Customer information validation
- Search query validation

### TypeScript Integration
- Strict TypeScript configuration
- Comprehensive type definitions
- Runtime validation with Zod
- Type-safe store operations

## 🚀 Deployment

The application is built as a Single Page Application (SPA) and can be deployed to any static hosting service:

1. Build the application:
   ```bash
   pnpm build
   ```

2. Deploy the `dist` folder to your hosting service

### Recommended Hosting Platforms
- Vercel
- Netlify
- AWS S3 + CloudFront
- GitHub Pages

## 🔮 Future Enhancements

### Planned Features
- [ ] Customer management system
- [ ] Inventory management
- [ ] Advanced analytics dashboard
- [ ] Receipt printing integration
- [ ] Barcode scanning
- [ ] Multi-location support
- [ ] Employee management
- [ ] Sales reporting
- [ ] Offline mode support
- [ ] API integration

### Technical Improvements
- [ ] Add E2E tests with Playwright
- [ ] Implement PWA features
- [ ] Add internationalization (i18n)
- [ ] Performance monitoring
- [ ] Error boundary implementation
- [ ] Advanced caching strategies

## 📖 API Documentation

### Store Actions

#### Cart Operations
```typescript
// Add product to cart
addToCart(product: Product, quantity?: number): void

// Remove product from cart
removeFromCart(productId: string): void

// Update product quantity
updateCartQuantity(productId: string, quantity: number): void

// Clear entire cart
clearCart(): void
```

#### Transaction Operations
```typescript
// Process payment and create transaction
processPayment(paymentMethod: PaymentMethod): Promise<Transaction>
```

#### Analytics
```typescript
// Get today's transactions
getTodaysTransactions(): Transaction[]

// Calculate today's revenue
getTodaysRevenue(): number
```

### Custom Hooks

#### useCart()
Returns cart state and operations with computed values.

#### useProducts()
Provides product filtering and search functionality.

#### useTransactions()
Handles transaction history and payment processing.

#### usePOSAnalytics()
Computes sales analytics and statistics.

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

### Code Style
- Use TypeScript for all new code
- Follow existing naming conventions
- Add JSDoc comments for public APIs
- Write tests for new features
- Use semantic commit messages

## 📄 License

This project is licensed under the MIT License. See LICENSE file for details.

## 🆘 Support

For questions or support:
1. Check the documentation above
2. Review existing issues
3. Create a new issue with detailed information
4. Include steps to reproduce any bugs

---

**Built with ❤️ using modern web technologies** 