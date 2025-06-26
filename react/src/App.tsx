import { Routes, Route, Navigate } from 'react-router-dom';
import { POSInterface } from '@/apps/pos/pages/POSInterface';
import { InventoryInterface } from '@/apps/inventory/pages/InventoryInterface';
import './App.css';

/**
 * Main App Component
 * 
 * Root component that sets up routing and navigation for the POS application.
 * Currently includes the main POS interface with plans for additional routes.
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
      </Routes>
    </div>
  );
}

export default App;
