# Inventory Movement Module

This module tracks all inventory movements and stock changes in the retail system.

## Components

### 1. InventoryMovement Entity
- Tracks individual inventory movements
- Records movement type, quantity, stock levels, and metadata
- Links to inventory items and users

### 2. MovementType Enum
Available movement types:
- `STOCK_IN` - Stock received/added
- `STOCK_OUT` - Stock removed/dispatched
- `SALE` - Item sold
- `PURCHASE` - Item purchased
- `RETURN` - Item returned
- `DAMAGE` - Item damaged/lost
- `EXPIRED` - Item expired
- `TRANSFER` - Stock transfer
- `MANUAL_ADJUSTMENT` - Manual stock adjustment

### 3. Service Layer
- `InventoryMovementService` - Business logic for recording and retrieving movements
- Automatically calculates new stock levels
- Updates inventory item stock through InventoryItemService

### 4. Controller Layer
- `InventoryMovementController` - REST API endpoints
- Designed for internal use by other controllers
- Supports filtering and pagination

## API Endpoints

### Record Movement
```
POST /api/inventory/movements
```

### Get Movement by ID
```
GET /api/inventory/movements/{id}
```

### Get Movements by Inventory Item
```
GET /api/inventory/movements/item/{itemId}
```

### Get Movements by Inventory
```
GET /api/inventory/movements/inventory/{inventoryId}
```

### Get Movements by Type
```
GET /api/inventory/movements/type/{type}
```

### Get Movements by User
```
GET /api/inventory/movements/user/{userId}
```

### Get Movements by Date Range
```
GET /api/inventory/movements/date-range?startDate={start}&endDate={end}
```

### Get Movements by Reference
```
GET /api/inventory/movements/reference/{referenceType}/{referenceId}
```

## Usage Example

```java
// Record a sale movement
CreateMovementRequest request = new CreateMovementRequest();
request.setUserId("user123");
request.setInventoryItemId("item456");
request.setMovementType(MovementType.SALE);
request.setQuantity(5);
request.setReason("Customer purchase");
request.setReferenceType("ORDER");
request.setReferenceId("order789");

InventoryMovement movement = movementService.recordMovement(request);
```

## Notes

- All movements are automatically persisted with timestamps
- Stock levels are automatically updated in the inventory item
- The module is designed for internal use by other controllers
- Supports pagination for large datasets
- Includes comprehensive filtering options