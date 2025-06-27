import { Routes, Route, Navigate } from 'react-router-dom';
import { POSInterface } from '@/apps/pos/pages/POSInterface';
import { InventoryInterface } from '@/apps/inventory/pages/InventoryInterface';
import { AuthInterface } from '@/apps/auth/pages/AuthInterface';
import './App.css';

/**
 * Main App Component
 * 
 * Root component that sets up routing and navigation for the retail application.
 * Includes POS interface, inventory management, and user authentication/admin dashboard.
 * 
 * @returns App component
 */
function App() {
  return (
    <div className="h-full bg-background">
      <Routes>
        {/* Main POS Route */}
        <Route path="*" element={<POSInterface />} />

        {/* Inventory Management Route */}
        <Route path="/inventory" element={<InventoryInterface />} />

        {/* User Management & Authentication Route */}
        <Route path="/admin" element={<AuthInterface />} />
      </Routes>
    </div>
  );
}

export default App;
