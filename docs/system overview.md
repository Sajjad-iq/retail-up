# Retail UP System Overview

This document provides a high-level overview of the Retail UP system, based on the C4 model diagrams. It covers the system context, main containers, and backend components, summarizing the architecture, main actors, technologies, and responsibilities.

---

## 1. System Context

**Actors:**
- **Ahmed:** Business Owner (Admin)
- **Ali:** Employee (Cashier)

**Core Systems:**
- **Web App:** Main user interface for both admin and employee roles.
- **OTP System:** Used for user validation and passwordless login.
- **Backend System:** Handles business logic, data, users, and operations.

**Key Interactions:**
- Users interact with the Web App.
- The Web App communicates with the Backend System.
- The Backend System interacts with the OTP System for authentication.
- The OTP System validates users and sends OTPs.

---

## 2. Container Diagram

**Main Containers:**
- **Web App (Single Page App):**
  - Built with React.
  - Provides full or limited control based on user roles and permissions.
- **Desktop App:**
  - Electron-based, installable on PC.
  - Used mainly on office laptops.
- **Backend System:**
  - Exposes APIs for all business logic and data management.
- **OTP System:**
  - Handles authentication and user validation.
- **Cloudflare:**
  - Acts as an API gateway/proxy for security and routing.
- **Databases:**
  - **Redis:** For caching data.
  - **Postgres:** For persistent data storage.

**Key Flows:**
- Users access the system via Web App or Desktop App.
- Both apps communicate with the backend through Cloudflare.
- Backend uses Redis for caching and Postgres for main data storage.
- Backend interacts with the OTP system for authentication.

---

## 3. Component Diagram (Backend Controllers)

The backend is organized into multiple controllers, each responsible for a specific domain:

- **Auth Controller:** Login, logout, and user permissions.
- **Users Controller:** Create, update, and delete users.
- **Organization Controller:** Manage organizations (create, update, delete).
- **Settings Controller:** Organization settings management.
- **Employee Controller:** Manage employees (CRUD).
- **Inventory Controller:** Manage inventory (create, update).
- **Items Controller:** Manage inventory items (CRUD).
- **Movements Controller:** Track inventory movements.
- **Inventory Analytics Controller:** Daily analytics of inventory and finances.
- **POS Controller:** Point of Sale, manages sales and inventory.
- **Customer Controller:** Manage customers (CRUD).
- **Cart Controller:** Manage the sales cart and selling process.
- **Payment Plan Controller:** Create payment plans.
- **Payment Plans Manager Controller:** Manage active plans, reminders, and analytics.
- **Permissions Controller:** Manage permissions (create, update, etc.).
- **Reporting Controller:** Generate reports for sales, inventory, employees, etc.

**Relationships:**
- Controllers interact to perform business operations (e.g., Inventory Controller uses Organization data, Items Controller uses Inventory data).
- POS and Cart controllers are central to the sales process.
- Analytics and Reporting controllers provide insights and reports.

---

## Summary Table

| Layer         | Main Elements/Containers         | Purpose/Role                                      |
|---------------|----------------------------------|---------------------------------------------------|
| Context       | Users, Web App, OTP, Backend     | High-level actors and systems                     |
| Container     | Web App, Desktop App, Backend, OTP, Cloudflare, Redis, Postgres | System deployment and technology choices          |
| Component     | API Controllers (Auth, Users, Inventory, POS, etc.) | Detailed backend business logic and responsibilities |

---

## Key Takeaways

- Multi-user retail management platform with web and desktop interfaces.
- Passwordless authentication using OTP.
- API-driven backend with clear separation of concerns via controllers.
- Redis for caching, Postgres for persistent storage.
- Cloudflare as API gateway/proxy.
- Designed for extensibility and clear role-based access.
