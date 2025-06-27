import { Routes, Route } from 'react-router-dom';
import { POSInterface } from '@/apps/pos/pages/POSInterface';
import { InventoryInterface } from '@/apps/inventory/pages/InventoryInterface';
import { AdministrationPage, LoginPage } from '@/apps/auth/pages';
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
    <Routes>
      {/* Login Route - Standalone without AppLayout */}
      <Route path="/login" element={<LoginPage />} />

      {/* Main Application Routes - Wrapped in AppLayout */}
      <Route path="/*" element={
        <AppLayout>
          <Routes>
            {/* Main POS Route */}
            <Route path="/" element={<POSInterface />} />

            {/* Inventory Management Route */}
            <Route path="/inventory" element={<InventoryInterface />} />

            {/* User Management & Authentication Route */}
            <Route path="/admin" element={<AdministrationPage />} />
          </Routes>
        </AppLayout>
      } />
    </Routes>
  );
}
// 
export default App;
