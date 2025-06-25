import React from 'react';
import { Routes, Route, Navigate } from 'react-router-dom';
import { POSInterface } from '@/apps/pos/pages/POSInterface';
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
        <Route path="/pos" element={<POSInterface />} />

        {/* Dashboard Route (placeholder for future development) */}
        <Route
          path="/dashboard"
          element={
            <div className="flex items-center justify-center h-screen">
              <div className="text-center">
                <h1 className="text-3xl font-bold mb-4">Dashboard</h1>
                <p className="text-muted-foreground">Coming soon...</p>
                <a
                  href="/pos"
                  className="text-primary hover:underline"
                >
                  Go to POS →
                </a>
              </div>
            </div>
          }
        />

        {/* Analytics Route (placeholder for future development) */}
        <Route
          path="/analytics"
          element={
            <div className="flex items-center justify-center h-screen">
              <div className="text-center">
                <h1 className="text-3xl font-bold mb-4">Analytics</h1>
                <p className="text-muted-foreground">Coming soon...</p>
                <a
                  href="/pos"
                  className="text-primary hover:underline"
                >
                  Go to POS →
                </a>
              </div>
            </div>
          }
        />

        {/* Settings Route (placeholder for future development) */}
        <Route
          path="/settings"
          element={
            <div className="flex items-center justify-center h-screen">
              <div className="text-center">
                <h1 className="text-3xl font-bold mb-4">Settings</h1>
                <p className="text-muted-foreground">Coming soon...</p>
                <a
                  href="/pos"
                  className="text-primary hover:underline"
                >
                  Go to POS →
                </a>
              </div>
            </div>
          }
        />

        {/* Default redirect to POS */}
        <Route path="/" element={<Navigate to="/pos" replace />} />

        {/* 404 Route */}
        <Route
          path="*"
          element={
            <div className="flex items-center justify-center h-screen">
              <div className="text-center">
                <h1 className="text-3xl font-bold mb-4">404 - Page Not Found</h1>
                <p className="text-muted-foreground mb-4">
                  The page you're looking for doesn't exist.
                </p>
                <a
                  href="/pos"
                  className="text-primary hover:underline"
                >
                  Go to POS →
                </a>
              </div>
            </div>
          }
        />
      </Routes>
    </div>
  );
}

export default App;
