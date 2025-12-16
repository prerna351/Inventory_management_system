# ABAP Inventory Management System

A simple, console-based Inventory Management System implemented using
modern ABAP Object-Oriented programming.  
The project is **ABAP Cloud compatible** and focuses on core ABAP language
concepts rather than enterprise frameworks.

---

## Overview

This project demonstrates how to design and execute a small but complete
ABAP application using object-oriented principles.  
The inventory is managed **in memory** using internal tables and is
executed via a runnable ABAP Cloud class.

The goal of this project is learning and showcasing **clean ABAP logic**

---

## Features

- Add inventory items with input validation  
- Search items by ID  
- Update item quantity  
- Delete items from inventory  
- Compute total number of items  
- Compute total stock quantity  
- Runnable using `IF_OO_ADT_CLASSRUN` (ABAP Cloud execution model)

---

## Technologies & Concepts Used

- Modern ABAP Object-Oriented programming
- Class-based design and encapsulation
- Internal tables for data storage
- CRUD operations (Create, Read, Update, Delete)
- `sy-subrc` handling for existence checks
- Assertions for input validation
- String templates for formatted console output
- Runnable ABAP Cloud classes (`IF_OO_ADT_CLASSRUN`)

---


---

## How to Run

1. Open the project in **Eclipse with ADT**
2. Activate the classes
3. Right-click on class `ZCL_INVENTORY_DEMO`
4. Select  
   **Run As → ABAP Application (Console)**
5. View the output in the ADT console

---

## Design Notes

- The inventory data is stored in an **internal table** (no database).
- Validation is performed using `ASSERT` to keep the logic simple and explicit.
- Errors such as operating on non-existing items are treated as logical errors.
- The project intentionally avoids RAP, CDS, and database persistence to focus
  on mastering ABAP fundamentals first.

---

## Learning Focus

This project was built to strengthen understanding of:

- ABAP execution flow in ABAP Cloud
- Method visibility and class interfaces
- Internal table operations
- Defensive programming
- Readable and explainable ABAP code

---

## Future Enhancements

- ABAP Unit tests
- Persistence using database tables
- RAP and CDS-based implementation
- Fiori or service-based exposure

---

## Disclaimer

This is a **learning and portfolio project** intended to demonstrate ABAP
programming skills. It is not intended for productive use without further
enhancements.


