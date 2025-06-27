import { Routes, Route } from 'react-router-dom';
import { POSInterface } from '@/apps/pos/pages/POSInterface';
import { InventoryInterface } from '@/apps/inventory/pages/InventoryInterface';
import { AuthInterface } from '@/apps/auth/pages/AuthInterface';
import { AppLayout } from '@/components/layout/AppLayout';
import './App.css';

/**
 * Main App Component
 * 
 * Root component that sets up routing and navigation for the retail application.
 * Includes POS interface, inventory management, and user authentication/admin dashboard.
 * Now features a sidebar navigation layout.
 * 
 * @returns App component
 */
function App() {
  return (
    <AppLayout>
      <Routes>
        {/* Main POS Route */}
        <Route path="/" element={<POSInterface />} />

        {/* Inventory Management Route */}
        <Route path="/inventory" element={<InventoryInterface />} />

        {/* User Management & Authentication Route */}
        <Route path="/admin" element={<AuthInterface />} />
      </Routes>
    </AppLayout>
  );
}

export default App;
